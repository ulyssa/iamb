//! Tab completions for iamb
use std::borrow::Cow;
use std::str::FromStr;

use modalkit::{
    editing::{
        completion::{Completer, complete_path},
        cursor::Cursor,
        rope::EditRope,
    },
    env::vim::command::CommandDescription,
    prelude::{CommandType, WordStyle},
};

use crate::base::{ChatStore, IambBufferId, IambInfo, MATRIX_ID_WORD, RoomFocus};

pub struct IambCompleter;

impl Completer<IambInfo> for IambCompleter {
    fn complete(
        &mut self,
        text: &EditRope,
        cursor: &mut Cursor,
        content: &IambBufferId,
        store: &mut ChatStore,
    ) -> Vec<String> {
        match content {
            IambBufferId::Command(CommandType::Command) => complete_cmdbar(text, cursor, store),
            IambBufferId::Command(CommandType::Search) => vec![],
            IambBufferId::Room(_, _, RoomFocus::MessageBar) => complete_msgbar(text, cursor, store),
            IambBufferId::Room(_, _, RoomFocus::Scrollback) => vec![],

            IambBufferId::DirectList => vec![],
            IambBufferId::MemberList(_) => vec![],
            IambBufferId::RoomList => vec![],
            IambBufferId::SpaceList => vec![],
            IambBufferId::VerifyList => vec![],
            IambBufferId::Welcome => vec![],
            IambBufferId::ChatList => vec![],
            IambBufferId::UnreadList => vec![],
            IambBufferId::MentionsList => vec![],
        }
    }
}

/// Tab completion for user IDs.
fn complete_users(text: &EditRope, cursor: &mut Cursor, store: &ChatStore) -> Vec<String> {
    let id = text
        .get_prefix_word_mut(cursor, &MATRIX_ID_WORD)
        .unwrap_or_else(EditRope::empty);
    let id = Cow::from(&id);

    store
        .presences
        .complete(id.as_ref())
        .into_iter()
        .map(|i| i.to_string())
        .collect()
}

/// Tab completion within the message bar.
fn complete_msgbar(text: &EditRope, cursor: &mut Cursor, store: &ChatStore) -> Vec<String> {
    let id = text
        .get_prefix_word_mut(cursor, &MATRIX_ID_WORD)
        .unwrap_or_else(EditRope::empty);
    let id = Cow::from(&id);

    match id.chars().next() {
        // Complete room aliases.
        Some('#') => {
            return store.names.complete(id.as_ref());
        },

        // Complete room identifiers.
        Some('!') => {
            return store
                .rooms
                .complete(id.as_ref())
                .into_iter()
                .map(|i| i.to_string())
                .collect();
        },

        // Complete Emoji shortcodes.
        Some(':') => {
            let list = store.emojis.complete(&id[1..]);
            let iter = list.into_iter().take(200).map(|s| format!(":{s}:"));

            return iter.collect();
        },

        // Complete usernames for @ and empty strings.
        Some('@') | None => {
            return store
                .presences
                .complete(id.as_ref())
                .into_iter()
                .map(|i| i.to_string())
                .collect();
        },

        // Unknown sigil.
        Some(_) => return vec![],
    }
}

/// Tab completion for Matrix identifiers (usernames, room aliases, etc.)
fn complete_matrix_names(text: &EditRope, cursor: &mut Cursor, store: &ChatStore) -> Vec<String> {
    let id = text
        .get_prefix_word_mut(cursor, &MATRIX_ID_WORD)
        .unwrap_or_else(EditRope::empty);
    let id = Cow::from(&id);

    let list = store.names.complete(id.as_ref());
    if !list.is_empty() {
        return list;
    }

    let list = store.presences.complete(id.as_ref());
    if !list.is_empty() {
        return list.into_iter().map(|i| i.to_string()).collect();
    }

    store
        .rooms
        .complete(id.as_ref())
        .into_iter()
        .map(|i| i.to_string())
        .collect()
}

/// Tab completion for Emoji shortcode names.
fn complete_emoji(text: &EditRope, cursor: &mut Cursor, store: &ChatStore) -> Vec<String> {
    let sc = text.get_prefix_word_mut(cursor, &WordStyle::Little);
    let sc = sc.unwrap_or_else(EditRope::empty);
    let sc = Cow::from(&sc);

    store.emojis.complete(sc.as_ref())
}

/// Tab completion for command names.
fn complete_cmdname(
    desc: CommandDescription,
    text: &EditRope,
    cursor: &mut Cursor,
    store: &ChatStore,
) -> Vec<String> {
    // Complete command name and set cursor position.
    let _ = text.get_prefix_word_mut(cursor, &WordStyle::Little);
    store.cmds.complete_name(desc.command.as_str())
}

/// Tab completion for command arguments.
fn complete_cmdarg(
    desc: CommandDescription,
    text: &EditRope,
    cursor: &mut Cursor,
    store: &ChatStore,
) -> Vec<String> {
    let cmd = match store.cmds.get(desc.command.as_str()) {
        Ok(cmd) => cmd,
        Err(_) => return vec![],
    };

    match cmd.name.as_str() {
        "cancel" | "dms" | "edit" | "redact" | "reply" => vec![],
        "members" | "rooms" | "spaces" | "welcome" => vec![],
        "download" | "keys" | "open" | "upload" => complete_path(text, cursor),
        "react" | "unreact" => complete_emoji(text, cursor, store),

        "invite" => complete_users(text, cursor, store),
        "join" | "split" | "vsplit" | "tabedit" => complete_matrix_names(text, cursor, store),
        "room" => vec![],
        "verify" => vec![],
        "vertical" | "horizontal" | "aboveleft" | "belowright" | "tab" => {
            complete_cmd(desc.arg.text.as_str(), text, cursor, store)
        },
        _ => vec![],
    }
}

/// Tab completion for commands.
fn complete_cmd(cmd: &str, text: &EditRope, cursor: &mut Cursor, store: &ChatStore) -> Vec<String> {
    match CommandDescription::from_str(cmd) {
        Ok(desc) => {
            if desc.arg.untrimmed.is_empty() {
                complete_cmdname(desc, text, cursor, store)
            } else {
                // Complete command argument.
                complete_cmdarg(desc, text, cursor, store)
            }
        },

        // Can't parse command text, so return zero completions.
        Err(_) => vec![],
    }
}

/// Tab completion for the command bar.
fn complete_cmdbar(text: &EditRope, cursor: &mut Cursor, store: &ChatStore) -> Vec<String> {
    let eo = text.cursor_to_offset(cursor);
    let slice = text.slice(..eo);
    let cow = Cow::from(&slice);

    complete_cmd(cow.as_ref(), text, cursor, store)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tests::*;
    use pretty_assertions::assert_eq;

    #[tokio::test]
    async fn test_complete_msgbar() {
        let store = mock_store().await;
        let store = store.application;

        let text = EditRope::from("going for a walk :walk ");
        let mut cursor = Cursor::new(0, 22);
        let res = complete_msgbar(&text, &mut cursor, &store);
        assert_eq!(res, vec![":walking:", ":walking_man:", ":walking_woman:"]);
        assert_eq!(cursor, Cursor::new(0, 17));

        let text = EditRope::from("hello @user1 ");
        let mut cursor = Cursor::new(0, 12);
        let res = complete_msgbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["@user1:example.com"]);
        assert_eq!(cursor, Cursor::new(0, 6));

        let text = EditRope::from("see #room ");
        let mut cursor = Cursor::new(0, 9);
        let res = complete_msgbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["#room1:example.com"]);
        assert_eq!(cursor, Cursor::new(0, 4));
    }

    #[tokio::test]
    async fn test_complete_cmdbar() {
        let store = mock_store().await;
        let store = store.application;
        let users = vec![
            "@user1:example.com",
            "@user2:example.com",
            "@user3:example.com",
            "@user4:example.com",
            "@user5:example.com",
        ];

        let text = EditRope::from("invite    ");
        let mut cursor = Cursor::new(0, 7);
        let id = text
            .get_prefix_word_mut(&mut cursor, &MATRIX_ID_WORD)
            .unwrap_or_else(EditRope::empty);
        assert_eq!(id.to_string(), "");
        assert_eq!(cursor, Cursor::new(0, 7));

        let text = EditRope::from("invite    ");
        let mut cursor = Cursor::new(0, 7);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, users);

        let text = EditRope::from("invite ignored");
        let mut cursor = Cursor::new(0, 7);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, users);

        let text = EditRope::from("invite @user1ignored");
        let mut cursor = Cursor::new(0, 13);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["@user1:example.com"]);

        let text = EditRope::from("abo hor");
        let mut cursor = Cursor::new(0, 7);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["horizontal"]);

        let text = EditRope::from("abo hor inv");
        let mut cursor = Cursor::new(0, 11);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, vec!["invite"]);

        let text = EditRope::from("abo hor invite \n");
        let mut cursor = Cursor::new(0, 15);
        let res = complete_cmdbar(&text, &mut cursor, &store);
        assert_eq!(res, users);
    }
}
