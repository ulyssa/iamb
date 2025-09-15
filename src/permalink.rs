//! # MSC4352: Customizable HTTPS permalink base URLs
//!
//! This module implements discovery and generation of customizable HTTPS permalinks
//! as specified in MSC4352. It allows organizations to use branded domains while
//! maintaining compatibility with the existing matrix.to path/query grammar.

use std::env;
use std::collections::HashMap;
use url::{Url, percent_encoding::{utf8_percent_encode, NON_ALPHANUMERIC}};
use serde::{Deserialize, Serialize};
use anyhow::{Result, Context};
use matrix_sdk::ruma::{OwnedRoomId, OwnedUserId, OwnedEventId};
use regex::Regex;

/// Configuration for MSC4352 feature
#[derive(Debug, Clone)]
pub struct Msc4352Config {
    /// Whether MSC4352 is enabled
    pub enabled: bool,
    /// Environment override for permalink base URL
    pub env_override: Option<String>,
}

impl Default for Msc4352Config {
    fn default() -> Self {
        Self {
            enabled: env::var("IAMB_MSC4352").map(|v| v == "1").unwrap_or(false),
            env_override: env::var("IAMB_MSC4352_PERMALINK_BASE").ok(),
        }
    }
}

/// Well-known client configuration for permalink discovery
#[derive(Debug, Deserialize, Serialize)]
struct WellKnownClient {
    #[serde(rename = "m.permalink_base_url")]
    stable_permalink_base_url: Option<String>,
    #[serde(rename = "org.matrix.msc4352.permalink_base_url")]
    unstable_permalink_base_url: Option<String>,
}

/// Matrix identifier types for permalinks
#[derive(Debug, Clone, PartialEq)]
pub enum MatrixIdentifier {
    RoomAlias(String),     // #alias:server
    RoomId(OwnedRoomId),   // !room:server
    UserId(OwnedUserId),   // @user:server
    GroupId(String),       // +group:server
}

impl MatrixIdentifier {
    /// Parse a Matrix identifier from a string
    pub fn parse(s: &str) -> Option<Self> {
        if let Some(stripped) = s.strip_prefix('#') {
            Some(MatrixIdentifier::RoomAlias(format!("#{}", stripped)))
        } else if let Some(stripped) = s.strip_prefix('!') {
            if let Ok(room_id) = format!("!{}", stripped).try_into() {
                Some(MatrixIdentifier::RoomId(room_id))
            } else {
                None
            }
        } else if let Some(stripped) = s.strip_prefix('@') {
            if let Ok(user_id) = format!("@{}", stripped).try_into() {
                Some(MatrixIdentifier::UserId(user_id))
            } else {
                None
            }
        } else if let Some(stripped) = s.strip_prefix('+') {
            Some(MatrixIdentifier::GroupId(format!("+{}", stripped)))
        } else {
            None
        }
    }

    /// Get the string representation of this identifier
    pub fn as_str(&self) -> &str {
        match self {
            MatrixIdentifier::RoomAlias(s) => s,
            MatrixIdentifier::RoomId(id) => id.as_str(),
            MatrixIdentifier::UserId(id) => id.as_str(),
            MatrixIdentifier::GroupId(s) => s,
        }
    }
}

/// Convert Matrix identifier and event to matrix: URI
pub fn convert_https_permalink_to_matrix_uri(url_str: &str) -> Option<String> {
    let url = Url::parse(url_str).ok()?;

    // Check if this looks like a resolver-style permalink
    let fragment = url.fragment()?;
    if !fragment.starts_with('/') {
        return None;
    }

    let path_parts: Vec<&str> = fragment[1..].split('/').collect();
    if path_parts.is_empty() {
        return None;
    }

    // Decode the identifier
    let identifier_encoded = path_parts[0];
    let identifier = percent_encoding::percent_decode_str(identifier_encoded)
        .decode_utf8()
        .ok()?;

    let matrix_id = MatrixIdentifier::parse(&identifier)?;

    // Handle event if present
    let event_id = if path_parts.len() > 1 {
        let event_encoded = path_parts[1];
        let event_decoded = percent_encoding::percent_decode_str(event_encoded)
            .decode_utf8()
            .ok()?;
        Some(event_decoded.to_string())
    } else {
        None
    };

    // Extract via parameters
    let via_params: Vec<String> = url.query_pairs()
        .filter(|(key, _)| key == "via")
        .map(|(_, value)| value.to_string())
        .collect();

    // Build matrix: URI
    let mut matrix_uri = match matrix_id {
        MatrixIdentifier::RoomAlias(alias) => {
            let alias_without_hash = alias.strip_prefix('#')?;
            format!("matrix:r/{}", alias_without_hash)
        },
        MatrixIdentifier::RoomId(room_id) => {
            format!("matrix:roomid/{}", room_id)
        },
        MatrixIdentifier::UserId(user_id) => {
            format!("matrix:u/{}", user_id)
        },
        MatrixIdentifier::GroupId(group_id) => {
            format!("matrix:g/{}", group_id)
        },
    };

    // Add event if present
    if let Some(event) = event_id {
        matrix_uri.push_str(&format!("/e/{}", event));
    }

    // Add via parameters
    if !via_params.is_empty() {
        matrix_uri.push('?');
        let via_query: Vec<String> = via_params.iter()
            .map(|server| format!("via={}", server))
            .collect();
        matrix_uri.push_str(&via_query.join("&"));
    }

    Some(matrix_uri)
}

/// Discover permalink base URL from homeserver's well-known
pub async fn discover_resolver_base(homeserver_domain: &str) -> Result<Option<String>> {
    let well_known_url = format!("https://{}/.well-known/matrix/client", homeserver_domain);

    let client = reqwest::Client::new();
    let response = client.get(&well_known_url)
        .timeout(std::time::Duration::from_secs(10))
        .send()
        .await
        .context("Failed to fetch .well-known/matrix/client")?;

    if !response.status().is_success() {
        return Ok(None);
    }

    let well_known: WellKnownClient = response.json()
        .await
        .context("Failed to parse .well-known/matrix/client JSON")?;

    // Check stable key first, then unstable
    let base_url = well_known.stable_permalink_base_url
        .or(well_known.unstable_permalink_base_url);

    if let Some(url_str) = base_url {
        // Validate it's a proper HTTPS URL
        if let Ok(url) = Url::parse(&url_str) {
            if url.scheme() == "https" {
                // Return just the origin (scheme + host + optional port)
                let origin = format!("{}://{}", url.scheme(), url.host_str().unwrap_or_default());
                let origin_with_port = if let Some(port) = url.port() {
                    format!("{}:{}", origin, port)
                } else {
                    origin
                };
                return Ok(Some(origin_with_port));
            }
        }
    }

    Ok(None)
}

/// Determine effective permalink base URL using precedence rules
pub fn effective_permalink_base(
    config_override: Option<&str>,
    discovered_base: Option<&str>,
) -> String {
    // Precedence: user override → well-known discovery → fallback to matrix.to
    config_override
        .or(discovered_base)
        .unwrap_or("https://matrix.to")
        .to_string()
}

/// Generate HTTPS permalink using matrix.to navigation grammar
pub fn make_https_permalink(
    base_origin: &str,
    identifier: &MatrixIdentifier,
    event_id: Option<&str>,
    via_servers: &[String],
) -> String {
    let mut url = format!("{}/#/{}",
        base_origin.trim_end_matches('/'),
        utf8_percent_encode(identifier.as_str(), NON_ALPHANUMERIC)
    );

    if let Some(event) = event_id {
        url.push('/');
        url.push_str(&utf8_percent_encode(event, NON_ALPHANUMERIC).to_string());
    }

    if !via_servers.is_empty() {
        url.push('?');
        let via_params: Vec<String> = via_servers.iter()
            .map(|server| format!("via={}", utf8_percent_encode(server, NON_ALPHANUMERIC)))
            .collect();
        url.push_str(&via_params.join("&"));
    }

    url
}

/// Convert outgoing text containing resolver-style permalinks to HTML with matrix: hrefs
pub fn linkify_outgoing_text_to_html(text: &str) -> Option<String> {
    // Regex to find HTTPS URLs that look like resolver-style permalinks
    let permalink_regex = Regex::new(r"https://[^/\s]+/#/[^\s]*").ok()?;

    let mut found_any = false;
    let mut result = String::new();
    let mut last_end = 0;

    for mat in permalink_regex.find_iter(text) {
        found_any = true;

        // Add text before this match
        result.push_str(&text[last_end..mat.start()]);

        let url_text = mat.as_str();

        // Try to convert to matrix: URI
        if let Some(matrix_uri) = convert_https_permalink_to_matrix_uri(url_text) {
            // HTML-escape the visible URL text
            let escaped_url = html_escape::encode_text(url_text);
            result.push_str(&format!(r#"<a href="{}">{}</a>"#, matrix_uri, escaped_url));
        } else {
            // If conversion fails, keep original text
            result.push_str(url_text);
        }

        last_end = mat.end();
    }

    if found_any {
        // Add remaining text
        result.push_str(&text[last_end..]);
        Some(result)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_matrix_identifier_parse() {
        assert_eq!(
            MatrixIdentifier::parse("#room:example.org"),
            Some(MatrixIdentifier::RoomAlias("#room:example.org".to_string()))
        );

        assert_eq!(
            MatrixIdentifier::parse("@user:example.org").unwrap(),
            MatrixIdentifier::UserId("@user:example.org".try_into().unwrap())
        );

        assert_eq!(
            MatrixIdentifier::parse("+group:example.org"),
            Some(MatrixIdentifier::GroupId("+group:example.org".to_string()))
        );
    }

    #[test]
    fn test_make_https_permalink() {
        let identifier = MatrixIdentifier::RoomAlias("#room:example.org".to_string());
        let result = make_https_permalink(
            "https://links.example.com",
            &identifier,
            None,
            &["example.org".to_string()],
        );

        assert!(result.starts_with("https://links.example.com/#/"));
        assert!(result.contains("via=example.org"));
    }

    #[test]
    fn test_convert_https_permalink_to_matrix_uri() {
        let result = convert_https_permalink_to_matrix_uri(
            "https://links.example.org/#/%23room%3Aexample.org?via=example.org"
        );

        assert_eq!(result, Some("matrix:r/room:example.org?via=example.org".to_string()));
    }

    #[test]
    fn test_linkify_outgoing_text_to_html() {
        let text = "Check out this room: https://links.example.org/#/%23room%3Aexample.org";
        let result = linkify_outgoing_text_to_html(text).unwrap();

        assert!(result.contains(r#"<a href="matrix:r/room:example.org""#));
        assert!(result.contains("https://links.example.org"));
    }

    #[test]
    fn test_effective_permalink_base() {
        assert_eq!(
            effective_permalink_base(Some("https://custom.example"), None),
            "https://custom.example"
        );

        assert_eq!(
            effective_permalink_base(None, Some("https://discovered.example")),
            "https://discovered.example"
        );

        assert_eq!(
            effective_permalink_base(None, None),
            "https://matrix.to"
        );
    }
}