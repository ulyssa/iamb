use std::net::{Ipv4Addr, Ipv6Addr};

use matrix_sdk::authentication::oauth::OAuthAuthorizationData;
use matrix_sdk::authentication::oauth::registration::{
    ApplicationType,
    ClientMetadata,
    Localized,
    OAuthGrantType,
};
use matrix_sdk::ruma;
use matrix_sdk::ruma::UserId;
use matrix_sdk::ruma::serde::Raw;
use matrix_sdk::utils::UrlOrQuery;
use matrix_sdk::utils::local_server::{LocalServerBuilder, LocalServerRedirectHandle};
use tokio::io::AsyncBufReadExt;
use tokio::task::JoinHandle;

use crate::config::SavedDevice;
use crate::prelude::*;

enum OAuthRedirectResponse {
    Server(UrlOrQuery),
    Stdin(UrlOrQuery),
}

pub async fn oauth_login(
    client: Client,
    user_hint: &UserId,
    saved_device: Option<SavedDevice>,
) -> IambResult<()> {
    let oauth = client.oauth();

    match oauth.server_metadata().await {
        Ok(server_metadata) => {
            tracing::info!(
                "Found OAuth 2.0 server metadata with issuer: {}",
                server_metadata.issuer
            );

            let device = if let Some(SavedDevice::OAuth { device_id, client_id, .. }) = saved_device
            {
                oauth.restore_registered_client(client_id.clone());
                Some(device_id)
            } else {
                None
            };

            let (redirect_uri, server_handle) = LocalServerBuilder::new().spawn().await?;
            let OAuthAuthorizationData { url, .. } = oauth
                .login(redirect_uri, device, Some(client_metadata().into()), None)
                .user_id_hint(user_hint)
                .build()
                .await
                .map_err(IambError::from)?;

            let opened =
                format!("The following URL should have been opened in your browser:\n    {url}");
            tokio::task::spawn_blocking(move || open::that(url.as_str()));
            println!("{opened}");
            println!(
                "\n\n\
                If the redirect fails to connect (i.e., iamb is not reachable from your browser), \
                the url (or just the query string) can be pasted here to complete the authorisation flow.\
                "
            );

            let url_or_query = wait_auth_code(server_handle).await?;
            oauth.finish_login(url_or_query).await.map_err(IambError::from)?;
            Ok(())
        },
        Err(error) => {
            if error.is_not_supported() {
                tracing::warn!("Homeserver doesn't advertise OAuth 2.0 metadata");
            } else {
                tracing::warn!("Error fetching OAuth 2.0 metadata : {error:?}");
            }
            Err(UIError::Failure(
                "Error fetching homeserver OAuth 2.0 metadata. \
                    Homeserver needs to support native OAuth 2.0 for oauth login to work"
                    .to_string(),
            ))
        },
    }
}

async fn wait_auth_code(server_handle: LocalServerRedirectHandle) -> IambResult<UrlOrQuery> {
    let (tx, mut rx) = tokio::sync::mpsc::channel(1);

    let server_tx = tx.clone();

    let server = tokio::spawn(async move {
        let server_resp = server_handle.await;
        match server_resp {
            Some(query) => {
                let _ = server_tx.try_send(OAuthRedirectResponse::Server(query.into()));
            },
            None => {
                println!(
                    "Failed to read query string from browser redirect. \
                    Please paste url in here:\n"
                );
            },
        }
    });

    let stdin_reader: JoinHandle<Result<(), tokio::io::Error>> = tokio::task::spawn(async move {
        let stdin = tokio::io::stdin();
        let mut reader = tokio::io::BufReader::new(stdin);
        let mut line = String::new();

        loop {
            reader.read_line(&mut line).await?;
            if let Ok(url) = Url::parse(&line) {
                let _ = tx.try_send(OAuthRedirectResponse::Stdin(UrlOrQuery::Url(url)));
                break;
            } else if line.starts_with('?') {
                let strip_qm = line.split_off(0);
                let _ = tx.try_send(OAuthRedirectResponse::Stdin(UrlOrQuery::Query(strip_qm)));
                break;
            } else if line.contains("code=") {
                let _ = tx.try_send(OAuthRedirectResponse::Stdin(UrlOrQuery::Query(line)));
                break;
            } else if !line.contains(char::is_whitespace) {
                println!("Unrecognised url or query string");
            } else {
                break;
            }
        }
        Ok(())
    });

    let received = rx.recv().await;

    match received {
        Some(OAuthRedirectResponse::Server(qry)) => {
            println!("Received authorisation code from browser. Press Enter to continue...");
            let _ = stdin_reader.await;
            Ok(qry)
        },
        Some(OAuthRedirectResponse::Stdin(url)) => {
            server.abort();
            println!("Using redirect from stdin. Localhost listener will be ignored.");
            Ok(url)
        },
        None => {
            Err(UIError::Failure("Failed to receive redirect from stdin or browser".to_string()))
        },
    }
}

// Taken from the sdk's example
fn client_metadata() -> Raw<ClientMetadata> {
    let ipv4_localhost_uri = Url::parse(&format!("http://{}/", Ipv4Addr::LOCALHOST))
        .expect("Couldn't parse IPv4 redirect URI");
    let ipv6_localhost_uri = Url::parse(&format!("http://[{}]/", Ipv6Addr::LOCALHOST))
        .expect("Couldn't parse IPv6 redirect URI");
    let client_uri =
        Localized::new(Url::parse("https://iamb.chat").expect("Couldn't parse client URI"), None);
    let logo_uri = Localized::new(
        Url::parse("https://iamb.chat/images/iamb.png").expect("Couldn't parse logo uri"),
        None,
    );

    let metadata = ruma::assign!(
        ClientMetadata::new(
            ApplicationType::Native,
            vec![OAuthGrantType::AuthorizationCode {
                    redirect_uris: vec![ipv4_localhost_uri, ipv6_localhost_uri],
                }],
        client_uri), {
            client_name: Some(Localized::new("iamb".to_owned(), [])),
            logo_uri: Some(logo_uri)
    });

    Raw::new(&metadata).expect("Couldn't serialize client metadata")
}
