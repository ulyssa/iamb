//! # MSC4352: Customizable HTTPS permalink base URLs
//!
//! This module implements discovery and generation of customizable HTTPS permalinks
//! as specified in MSC4352. It allows organizations to use branded domains while
//! maintaining compatibility with the existing matrix.to path/query grammar.

use std::env;
use url::Url;
use serde::{Deserialize, Serialize};
use anyhow::{Result, Context};
use matrix_sdk::ruma::MatrixToUri;
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

/// Convert HTTPS permalink to matrix: URI
pub fn convert_https_permalink_to_matrix_uri(url_str: &str) -> Option<String> {
    // Parse the URL
    let url = Url::parse(url_str).ok()?;

    // Check if this looks like a resolver-style permalink
    let fragment = url.fragment()?;
    if !fragment.starts_with('/') {
        return None;
    }

    // Remove the leading slash from fragment
    let stripped = &fragment[1..];

    // Convert to matrix: URI format based on the identifier type
    let matrix_uri_str = if stripped.starts_with('#') {
        // Room alias - Example: #room:example.org -> matrix:r/room:example.org
        format!("matrix:r/{}", &stripped[1..])
    } else if stripped.starts_with('!') {
        // Room ID - Example: !roomid:example.org -> matrix:roomid/!roomid:example.org
        format!("matrix:roomid/{}", stripped)
    } else if stripped.starts_with('@') {
        // User ID - Example: @user:example.org -> matrix:u/user:example.org
        format!("matrix:u/{}", &stripped[1..])
    } else {
        return None;
    };

    // Add query parameters if present
    if let Some(query) = url.query() {
        Some(format!("{}?{}", matrix_uri_str, query))
    } else {
        Some(matrix_uri_str)
    }
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

/// Generate HTTPS permalink using custom base URL
pub fn make_https_permalink_with_base(
    base_origin: &str,
    matrix_to_uri: &MatrixToUri,
) -> String {
    // Get the string representation and replace the base URL
    let matrix_to_str = matrix_to_uri.to_string();

    // Replace https://matrix.to with custom base
    if let Some(stripped) = matrix_to_str.strip_prefix("https://matrix.to") {
        format!("{}{}", base_origin.trim_end_matches('/'), stripped)
    } else {
        matrix_to_str
    }
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
    fn test_convert_https_permalink_to_matrix_uri() {
        // Test standard matrix.to URL
        let result = convert_https_permalink_to_matrix_uri(
            "https://matrix.to/#/#room:example.org"
        );
        assert!(result.is_some());
        assert!(result.unwrap().starts_with("matrix:"));

        // Test custom base URL
        let result = convert_https_permalink_to_matrix_uri(
            "https://links.example.org/#/#room:example.org?via=example.org"
        );
        assert!(result.is_some());
        assert!(result.unwrap().starts_with("matrix:"));
    }

    #[test]
    fn test_linkify_outgoing_text_to_html() {
        // This test only works when MSC4352 is enabled, but linkify_outgoing_text_to_html
        // doesn't check the config - that's done by the caller.
        // So we test the function directly.
        let text = "Check out this room: https://links.example.org/#/#room:example.org";
        let result = linkify_outgoing_text_to_html(text);

        // The function should find and convert the permalink
        assert!(result.is_some());
        let html = result.unwrap();
        assert!(html.contains(r#"<a href="matrix:"#));
        assert!(html.contains("https://links.example.org"));
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

    #[test]
    fn test_room_alias_conversion() {
        // Test room alias: #room:example.org
        let url = "https://links.example.org/#/#room:example.org";
        let result = convert_https_permalink_to_matrix_uri(url).unwrap();
        assert_eq!(result, "matrix:r/room:example.org");
    }

    #[test]
    fn test_room_id_conversion() {
        // Test room ID: !roomid:example.org
        let url = "https://custom.resolver/#/!roomid:example.org";
        let result = convert_https_permalink_to_matrix_uri(url).unwrap();
        assert_eq!(result, "matrix:roomid/!roomid:example.org");
    }

    #[test]
    fn test_user_id_conversion() {
        // Test user ID: @user:example.org
        let url = "https://matrix.to/#/@user:example.org";
        let result = convert_https_permalink_to_matrix_uri(url).unwrap();
        assert_eq!(result, "matrix:u/user:example.org");
    }

    #[test]
    fn test_event_permalink_with_query() {
        // Test with query parameters
        let url = "https://matrix.to/#/#room:example.org?via=server1.org&via=server2.org";
        let result = convert_https_permalink_to_matrix_uri(url).unwrap();
        // Should preserve query parameters
        assert_eq!(result, "matrix:r/room:example.org?via=server1.org&via=server2.org");
    }

    #[test]
    fn test_invalid_permalink_no_conversion() {
        // Test invalid permalink (no fragment)
        let url = "https://example.com/page";
        let result = convert_https_permalink_to_matrix_uri(url);
        assert!(result.is_none());

        // Test invalid permalink (fragment doesn't start with /)
        let url2 = "https://example.com/#anchor";
        let result2 = convert_https_permalink_to_matrix_uri(url2);
        assert!(result2.is_none());
    }

    #[test]
    fn test_linkify_with_room_id() {
        // Test linkify with room ID
        let text = "Join this room: https://matrix.to/#/!roomid:example.org";
        let result = linkify_outgoing_text_to_html(text).unwrap();

        // Should contain correct matrix: URI with ! preserved
        assert!(result.contains(r#"href="matrix:roomid/!roomid:example.org""#));
        assert!(result.contains("https://matrix.to/#/!roomid:example.org"));
    }

    #[test]
    fn test_linkify_mixed_identifiers() {
        // Test with different identifier types
        let text = "User https://matrix.to/#/@user:example.org room https://links.org/#/!room:example.org";
        let result = linkify_outgoing_text_to_html(text).unwrap();

        // Check both conversions
        assert!(result.contains("matrix:u/user:example.org"));
        assert!(result.contains("matrix:roomid/!room:example.org"));
    }
}