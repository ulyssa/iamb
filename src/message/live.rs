//! Live messaging support for Matrix (MSC4357)
//!
//! This module implements live message composition where messages are sent
//! as they're being typed and continuously updated until finalized.

use std::collections::HashMap;
use std::time::{Duration, Instant};
use matrix_sdk::ruma::{EventId, OwnedEventId, OwnedRoomId};
use serde_json::{json, Value};

/// Minimum interval between live message updates
const MIN_UPDATE_INTERVAL: Duration = Duration::from_secs(2);

/// Rate limit window for tracking update frequency
const RATE_LIMIT_WINDOW: Duration = Duration::from_secs(60);

/// Maximum updates allowed per window
const MAX_UPDATES_PER_WINDOW: usize = 30;

/// The live message marker key used in event content
pub const LIVE_MESSAGE_MARKER: &str = "org.matrix.msc4357.live";

/// Live message session state
#[derive(Debug, Clone)]
pub struct LiveMessageSession {
    /// The event ID of the initial message
    pub event_id: OwnedEventId,

    /// Current content of the message
    pub current_content: String,

    /// Timestamp of the last update sent
    pub last_update: Instant,

    /// Number of updates sent for this message
    pub update_count: usize,

    /// Whether this session is still active
    pub is_active: bool,
}

/// Rate limiter to prevent excessive updates
#[derive(Debug)]
struct RateLimiter {
    /// Track update counts per room
    update_counts: HashMap<OwnedRoomId, Vec<Instant>>,
}

impl RateLimiter {
    fn new() -> Self {
        Self {
            update_counts: HashMap::new(),
        }
    }

    fn can_send_update(&mut self, room_id: &OwnedRoomId) -> bool {
        let now = Instant::now();
        let updates = self.update_counts.entry(room_id.clone()).or_default();

        // Remove old updates outside the window
        updates.retain(|&t| now.duration_since(t) < RATE_LIMIT_WINDOW);

        // Check if we're under the limit
        updates.len() < MAX_UPDATES_PER_WINDOW
    }

    fn record_update(&mut self, room_id: OwnedRoomId) {
        let updates = self.update_counts.entry(room_id).or_default();
        updates.push(Instant::now());
    }
}

/// Manager for live message sessions
#[derive(Debug)]
pub struct LiveMessageManager {
    /// Active live message sessions per room
    sessions: HashMap<OwnedRoomId, LiveMessageSession>,

    /// Rate limiter for updates
    rate_limiter: RateLimiter,

    /// Whether live messaging is enabled
    pub enabled: bool,
}

impl LiveMessageManager {
    /// Create a new live message manager
    pub fn new() -> Self {
        Self {
            sessions: HashMap::new(),
            rate_limiter: RateLimiter::new(),
            enabled: false,
        }
    }

    /// Start a new live message session
    pub fn start_session(&mut self, room_id: OwnedRoomId, initial_content: String) -> Option<LiveMessageSession> {
        tracing::info!("[LIVE] LiveMessageManager::start_session called, enabled={}", self.enabled);
        if !self.enabled {
            tracing::warn!("[LIVE] LiveMessageManager is disabled, cannot start session");
            return None;
        }

        // Generate temporary event ID - use a valid format
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis();
        let temp_event_id = format!("$temp_{}_{}", timestamp, room_id.as_str().replace(":", "_"));
        let event_id = temp_event_id.parse().ok()?;

        let session = LiveMessageSession {
            event_id,
            current_content: initial_content,
            last_update: Instant::now(),
            update_count: 0,
            is_active: true,
        };

        self.sessions.insert(room_id.clone(), session.clone());
        self.rate_limiter.record_update(room_id);

        Some(session)
    }

    /// Set the real event ID received from server
    pub fn set_event_id(&mut self, room_id: &OwnedRoomId, event_id: OwnedEventId) {
        if let Some(session) = self.sessions.get_mut(room_id) {
            session.event_id = event_id;
        }
    }

    /// Check if we still have a temporary event ID
    pub fn has_temp_event_id(&self, room_id: &OwnedRoomId) -> bool {
        self.sessions
            .get(room_id)
            .map(|s| s.event_id.as_str().starts_with("$temp_") || s.event_id.as_str().contains("local"))
            .unwrap_or(false)
    }

    /// Update an existing session
    pub fn update_session(&mut self, room_id: &OwnedRoomId, new_content: String) -> Option<LiveMessageSession> {
        if !self.enabled {
            return None;
        }

        let session = self.sessions.get_mut(room_id)?;

        if !session.is_active {
            return None;
        }

        session.current_content = new_content;
        session.last_update = Instant::now();
        session.update_count += 1;

        self.rate_limiter.record_update(room_id.clone());

        Some(session.clone())
    }

    /// Check if we should send an update now
    pub fn should_send_update(&mut self, room_id: &OwnedRoomId) -> bool {
        if !self.enabled {
            return false;
        }

        let session = match self.sessions.get(room_id) {
            Some(s) if s.is_active => s,
            _ => return false,
        };

        // Don't send if we have a temp ID
        if self.has_temp_event_id(room_id) {
            return false;
        }

        // Check minimum interval
        if session.last_update.elapsed() < MIN_UPDATE_INTERVAL {
            return false;
        }

        // Check rate limit
        self.rate_limiter.can_send_update(room_id)
    }

    /// Get the current session for a room
    pub fn get_session(&self, room_id: &OwnedRoomId) -> Option<&LiveMessageSession> {
        self.sessions.get(room_id)
    }

    /// End a live message session
    pub fn end_session(&mut self, room_id: &OwnedRoomId) -> Option<LiveMessageSession> {
        self.sessions.remove(room_id)
    }
}

/// Create initial live message JSON with fields
pub fn create_initial_live_message_with_fields(
    content: &str,
    custom_fields: Option<Value>,
) -> Value {
    let mut json = json!({
        "msgtype": "m.text",
        "body": content,
        LIVE_MESSAGE_MARKER: true
    });

    if let Some(custom) = custom_fields {
        if let Some(obj) = json.as_object_mut() {
            if let Some(custom_obj) = custom.as_object() {
                for (key, value) in custom_obj {
                    if !matches!(key.as_str(), "msgtype" | "body") {
                        obj.insert(key.clone(), value.clone());
                    }
                }
            }
        }
    }

    json
}

/// Create a live message update JSON with custom fields
pub fn create_live_update_json_with_fields(
    original_event_id: &EventId,
    new_content: &str,
    is_final: bool,
    custom_fields: Option<Value>,
) -> Value {
    let mut json = json!({
        "msgtype": "m.text",
        "body": format!("* {}", new_content),
        "m.new_content": {
            "msgtype": "m.text",
            "body": new_content,
        },
        "m.relates_to": {
            "rel_type": "m.replace",
            "event_id": original_event_id.to_string(),
        }
    });

    // Add live marker to new_content unless it's final
    if !is_final {
        if let Some(new_content_obj) = json["m.new_content"].as_object_mut() {
            new_content_obj.insert(LIVE_MESSAGE_MARKER.to_string(), json!(true));
            tracing::debug!("[LIVE JSON] Added live marker to update");
        }
    } else {
        tracing::info!("[LIVE JSON] Creating FINAL update WITHOUT live marker");
    }

    // Add custom fields if provided
    if let Some(custom) = custom_fields {
        if let Some(obj) = json.as_object_mut() {
            if let Some(custom_obj) = custom.as_object() {
                for (key, value) in custom_obj {
                    if !matches!(key.as_str(), "msgtype" | "body" | "m.new_content" | "m.relates_to") {
                        obj.insert(key.clone(), value.clone());
                    }
                }
            }
        }
    }

    json
}

/// Send a new live message with custom fields
pub async fn send_live_message_with_custom_fields(
    room: &matrix_sdk::Room,
    content: &str,
    custom_fields: Option<Value>,
) -> Result<matrix_sdk::ruma::OwnedEventId, matrix_sdk::Error> {
    let json = create_initial_live_message_with_fields(content, custom_fields);

    // Log for debugging
    tracing::debug!("Sending initial live message JSON: {}", serde_json::to_string_pretty(&json).unwrap_or_default());

    let resp = room.send_raw("m.room.message", json).await?;
    Ok(resp.event_id)
}

/// Send a live update with custom fields
pub async fn send_live_update_with_custom_fields(
    room: &matrix_sdk::Room,
    original_event_id: &EventId,
    new_content: &str,
    is_final: bool,
    custom_fields: Option<Value>,
) -> Result<matrix_sdk::ruma::OwnedEventId, matrix_sdk::Error> {
    let json = create_live_update_json_with_fields(
        original_event_id,
        new_content,
        is_final,
        custom_fields,
    );

    // Log for debugging
    tracing::debug!("Sending live update JSON: {}", serde_json::to_string_pretty(&json).unwrap_or_default());

    let resp = room.send_raw("m.room.message", json).await?;
    Ok(resp.event_id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_initial_live_message() {
        let content = "Hello world";
        let json = create_initial_live_message_with_fields(content, None);

        // Check that the JSON has the required fields
        assert_eq!(json["msgtype"], "m.text");
        assert_eq!(json["body"], "Hello world");
        assert_eq!(json["org.matrix.msc4357.live"], true);
    }

    #[test]
    fn test_create_live_update_with_marker() {
        use std::str::FromStr;
        let event_id = OwnedEventId::from_str("$test_event_id:example.org").unwrap();
        let content = "Updated content";
        let json = create_live_update_json_with_fields(&event_id, content, false, None);

        // Check update structure
        assert_eq!(json["msgtype"], "m.text");
        assert!(json["body"].as_str().unwrap().starts_with("* "));
        assert_eq!(json["m.new_content"]["body"], "Updated content");
        assert_eq!(json["m.new_content"]["org.matrix.msc4357.live"], true);
        assert_eq!(json["m.relates_to"]["rel_type"], "m.replace");
        assert_eq!(json["m.relates_to"]["event_id"], "$test_event_id:example.org");
    }

    #[test]
    fn test_create_final_update_without_marker() {
        use std::str::FromStr;
        let event_id = OwnedEventId::from_str("$test_event_id:example.org").unwrap();
        let content = "Final content";
        let json = create_live_update_json_with_fields(&event_id, content, true, None);

        // Check that final update doesn't have live marker
        assert_eq!(json["m.new_content"]["body"], "Final content");
        assert!(json["m.new_content"]["org.matrix.msc4357.live"].is_null());
    }

    #[test]
    fn test_live_message_manager_session() {
        use std::str::FromStr;
        let mut manager = LiveMessageManager::new();
        manager.enabled = true;

        let room_id = OwnedRoomId::from_str("!test:example.org").unwrap();
        let content = "Test message".to_string();

        // Start a session
        let session = manager.start_session(room_id.clone(), content.clone());
        assert!(session.is_some());

        let session = session.unwrap();
        assert_eq!(session.current_content, "Test message");
        assert!(session.is_active);

        // Get the session
        let retrieved = manager.get_session(&room_id);
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().current_content, "Test message");

        // End the session
        let ended = manager.end_session(&room_id);
        assert!(ended.is_some());

        // Session should be gone
        assert!(manager.get_session(&room_id).is_none());
    }
}