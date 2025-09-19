//! Live message detection utilities
//!
//! This module provides centralized logic for detecting whether a Matrix event
//! contains the MSC4357 live message marker.

use matrix_sdk::ruma::serde::Raw;
use serde_json::Value;

/// The MSC4357 live message marker key
pub const LIVE_MARKER: &str = "org.matrix.msc4357.live";

/// Check if a raw JSON string contains the live message marker
///
/// This function checks for the presence of the live marker in:
/// 1. Root level (for initial live messages)
/// 2. Inside m.new_content (for replacement/edit messages)
///
/// # Arguments
/// * `json_str` - The raw JSON string of the event
///
/// # Returns
/// * `true` if the event has the live marker set to true
/// * `false` otherwise
pub fn has_live_marker_in_json(json_str: &str) -> bool {
    // Quick string check first for performance
    if !json_str.contains(LIVE_MARKER) {
        return false;
    }

    // Parse JSON and check properly
    if let Ok(json) = serde_json::from_str::<Value>(json_str) {
        has_live_marker_in_value(&json)
    } else {
        // Fallback to simple string search if parsing fails
        json_str.contains(&format!(r#""{}":true"#, LIVE_MARKER))
    }
}

/// Check if a JSON value contains the live message marker
///
/// Checks both at root level and inside m.new_content for replacements
pub fn has_live_marker_in_value(json: &Value) -> bool {
    // Check in m.new_content first (for replacements)
    if let Some(new_content) = json.get("m.new_content") {
        if let Some(marker) = new_content.get(LIVE_MARKER) {
            return marker.as_bool().unwrap_or(false);
        }
    }

    // Check at root level (for initial messages)
    if let Some(marker) = json.get(LIVE_MARKER) {
        return marker.as_bool().unwrap_or(false);
    }

    // Also check in content field (for sync events)
    if let Some(content) = json.get("content") {
        // Check in content.m.new_content for replacements
        if let Some(new_content) = content.get("m.new_content") {
            if let Some(marker) = new_content.get(LIVE_MARKER) {
                return marker.as_bool().unwrap_or(false);
            }
        }
        // Check in content root for initial messages
        if let Some(marker) = content.get(LIVE_MARKER) {
            return marker.as_bool().unwrap_or(false);
        }
    }

    false
}

/// Check if a Raw event contains the live message marker
pub fn has_live_marker_in_raw<T>(raw: &Raw<T>) -> bool {
    has_live_marker_in_json(raw.json().get())
}

/// Determine if a replacement event is a live update or final update
///
/// Returns Some(true) if it's a live update, Some(false) if it's final,
/// or None if it's not a replacement
#[allow(dead_code)]
pub fn is_live_replacement(json: &Value) -> Option<bool> {
    // Check if this is a replacement
    let is_replacement = json.get("m.relates_to")
        .and_then(|r| r.get("rel_type"))
        .and_then(|t| t.as_str())
        .map(|t| t == "m.replace")
        .unwrap_or(false);

    if !is_replacement {
        // Also check in content.m.relates_to for sync events
        let is_replacement_in_content = json.get("content")
            .and_then(|c| c.get("m.relates_to"))
            .and_then(|r| r.get("rel_type"))
            .and_then(|t| t.as_str())
            .map(|t| t == "m.replace")
            .unwrap_or(false);

        if !is_replacement_in_content {
            return None;
        }
    }

    // It's a replacement, check if it has the live marker
    Some(has_live_marker_in_value(json))
}

/// Extract the target event ID from a replacement event
#[allow(dead_code)]
pub fn get_replacement_target_id(json: &Value) -> Option<String> {
    // Check in m.relates_to
    if let Some(event_id) = json.get("m.relates_to")
        .and_then(|r| r.get("event_id"))
        .and_then(|id| id.as_str()) {
        return Some(event_id.to_string());
    }

    // Check in content.m.relates_to for sync events
    json.get("content")
        .and_then(|c| c.get("m.relates_to"))
        .and_then(|r| r.get("event_id"))
        .and_then(|id| id.as_str())
        .map(|id| id.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_has_live_marker_in_json_root() {
        let json = json!({
            "msgtype": "m.text",
            "body": "Hello",
            "org.matrix.msc4357.live": true
        });
        let json_str = serde_json::to_string(&json).unwrap();
        assert!(has_live_marker_in_json(&json_str));
    }

    #[test]
    fn test_has_live_marker_in_json_new_content() {
        let json = json!({
            "msgtype": "m.text",
            "body": "* Hello",
            "m.new_content": {
                "msgtype": "m.text",
                "body": "Hello",
                "org.matrix.msc4357.live": true
            },
            "m.relates_to": {
                "rel_type": "m.replace",
                "event_id": "$test"
            }
        });
        let json_str = serde_json::to_string(&json).unwrap();
        assert!(has_live_marker_in_json(&json_str));
    }

    #[test]
    fn test_has_live_marker_in_sync_event() {
        let json = json!({
            "content": {
                "msgtype": "m.text",
                "body": "Hello",
                "org.matrix.msc4357.live": true
            },
            "event_id": "$test"
        });
        assert!(has_live_marker_in_value(&json));
    }

    #[test]
    fn test_no_live_marker() {
        let json = json!({
            "msgtype": "m.text",
            "body": "Hello"
        });
        let json_str = serde_json::to_string(&json).unwrap();
        assert!(!has_live_marker_in_json(&json_str));
    }

    #[test]
    fn test_is_live_replacement() {
        let live_replacement = json!({
            "m.new_content": {
                "body": "Hello",
                "org.matrix.msc4357.live": true
            },
            "m.relates_to": {
                "rel_type": "m.replace",
                "event_id": "$test"
            }
        });
        assert_eq!(is_live_replacement(&live_replacement), Some(true));

        let final_replacement = json!({
            "m.new_content": {
                "body": "Hello"
            },
            "m.relates_to": {
                "rel_type": "m.replace",
                "event_id": "$test"
            }
        });
        assert_eq!(is_live_replacement(&final_replacement), Some(false));

        let not_replacement = json!({
            "msgtype": "m.text",
            "body": "Hello"
        });
        assert_eq!(is_live_replacement(&not_replacement), None);
    }
}