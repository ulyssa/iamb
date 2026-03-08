#![allow(unused)]

use std::path::PathBuf;
use std::{collections::HashMap, sync::Weak};

use matrix_sdk::ruma::{
    event_id,
    events::room::message::RoomMessageEventContent,
    server_name,
    user_id,
    EventId,
    OwnedEventId,
    OwnedRoomId,
    OwnedUserId,
    RoomId,
};

use lazy_static::lazy_static;
use ratatui::style::{Color, Style};
use tokio::sync::mpsc::unbounded_channel;
use tracing::Level;
use url::Url;

use crate::{
    base::{ChatStore, ProgramStore, RoomInfo},
    config::{
        user_color,
        user_style_from_color,
        ApplicationSettings,
        DirectoryValues,
        Notifications,
        NotifyVia,
        ProfileConfig,
        SortOverrides,
        TunableValues,
        UserColor,
        UserDisplayStyle,
        UserDisplayTunables,
    },
    message::{Message, MessageKey, Messages},
    worker::Requester,
};

const TEST_ROOM1_ALIAS: &str = "#room1:example.com";

lazy_static! {
    pub static ref TEST_ROOM1_ID: OwnedRoomId =
        RoomId::new_v1(server_name!("example.com")).to_owned();
    pub static ref TEST_USER1: OwnedUserId = user_id!("@user1:example.com").to_owned();
    pub static ref TEST_USER2: OwnedUserId = user_id!("@user2:example.com").to_owned();
    pub static ref TEST_USER3: OwnedUserId = user_id!("@user3:example.com").to_owned();
    pub static ref TEST_USER4: OwnedUserId = user_id!("@user4:example.com").to_owned();
    pub static ref TEST_USER5: OwnedUserId = user_id!("@user5:example.com").to_owned();
    pub static ref MSG1_EVID: OwnedEventId = EventId::new(server_name!("example.com"));
    pub static ref MSG2_EVID: OwnedEventId = EventId::new(server_name!("example.com"));
    pub static ref MSG3_EVID: OwnedEventId =
        event_id!("$5jRz3KfVhaUzXtVj7k:example.com").to_owned();
    pub static ref MSG4_EVID: OwnedEventId =
        event_id!("$JP6qFV7WyXk5ZnexM3:example.com").to_owned();
    pub static ref MSG5_EVID: OwnedEventId = EventId::new(server_name!("example.com"));
    pub static ref MSG1_KEY: MessageKey = todo!();
    pub static ref MSG2_KEY: MessageKey = todo!();
    pub static ref MSG3_KEY: MessageKey = todo!();
    pub static ref MSG4_KEY: MessageKey = todo!();
    pub static ref MSG5_KEY: MessageKey = todo!();
}

pub fn user_style(user: &str) -> Style {
    user_style_from_color(user_color(user))
}

pub fn mock_room1_message(
    _content: RoomMessageEventContent,
    _sender: OwnedUserId,
    _key: MessageKey,
) -> Message {
    todo!()
}

pub fn mock_message1() -> Message {
    // let content = RoomMessageEventContent::text_plain("writhe");
    // let content = MessageEvent::Local(MSG1_EVID.clone(), content.into());

    todo!()
}

pub fn mock_message2() -> Message {
    let content = RoomMessageEventContent::text_plain("helium");

    mock_room1_message(content, TEST_USER2.clone(), MSG2_KEY.clone())
}

pub fn mock_message3() -> Message {
    let content = RoomMessageEventContent::text_plain("this\nis\na\nmultiline\nmessage");

    mock_room1_message(content, TEST_USER2.clone(), MSG3_KEY.clone())
}

pub fn mock_message4() -> Message {
    let content = RoomMessageEventContent::text_plain("help");

    mock_room1_message(content, TEST_USER1.clone(), MSG4_KEY.clone())
}

pub fn mock_message5() -> Message {
    let content = RoomMessageEventContent::text_plain("character");

    mock_room1_message(content, TEST_USER2.clone(), MSG4_KEY.clone())
}

// pub fn mock_keys() -> HashMap<OwnedEventId, EventLocation> {
//     let mut keys = HashMap::new();
//
//     keys.insert(MSG1_EVID.clone(), EventLocation::Message(None, MSG1_KEY.clone()));
//     keys.insert(MSG2_EVID.clone(), EventLocation::Message(None, MSG2_KEY.clone()));
//     keys.insert(MSG3_EVID.clone(), EventLocation::Message(None, MSG3_KEY.clone()));
//     keys.insert(MSG4_EVID.clone(), EventLocation::Message(None, MSG4_KEY.clone()));
//     keys.insert(MSG5_EVID.clone(), EventLocation::Message(None, MSG5_KEY.clone()));
//
//     keys
// }

pub fn mock_messages() -> Messages {
    todo!()
}

pub fn mock_room() -> RoomInfo {
    // let mut room = RoomInfo::default();
    // room.name = Some("Watercooler Discussion".into());
    // room.keys = mock_keys();
    // *room.get_thread_mut(None) = mock_messages();
    todo!()
}

pub fn mock_dirs() -> DirectoryValues {
    DirectoryValues {
        cache: PathBuf::new(),
        data: PathBuf::new(),
        logs: PathBuf::new(),
        downloads: None,
    }
}

pub fn mock_tunables() -> TunableValues {
    TunableValues {
        default_room: None,
        log_level: Level::INFO,
        message_shortcode_display: false,
        normal_after_send: true,
        reaction_display: true,
        reaction_shortcode_display: false,
        read_receipt_send: true,
        read_receipt_display: true,
        request_timeout: 120,
        sort: SortOverrides::default().values(),
        state_event_display: true,
        typing_notice_send: true,
        typing_notice_display: true,
        users: vec![(TEST_USER5.clone(), UserDisplayTunables {
            color: Some(UserColor(Color::Black)),
            name: Some("USER 5".into()),
        })]
        .into_iter()
        .collect::<HashMap<_, _>>(),
        open_command: None,
        external_edit_file_suffix: String::from(".md"),
        username_display: UserDisplayStyle::Username,
        message_user_color: false,
        mouse: Default::default(),
        notifications: Notifications {
            enabled: false,
            via: NotifyVia::default(),
            show_message: true,
            sound_hint: None,
        },
        image_preview: None,
        user_gutter_width: 30,
        tabstop: 4,
    }
}

pub fn mock_settings() -> ApplicationSettings {
    ApplicationSettings {
        layout_json: PathBuf::new(),
        session_json: PathBuf::new(),
        session_json_old: PathBuf::new(),
        sled_dir: PathBuf::new(),
        sqlite_dir: PathBuf::new(),

        profile_name: "test".into(),
        profile: ProfileConfig {
            user_id: user_id!("@user:example.com").to_owned(),
            url: None,
            settings: None,
            dirs: None,
            layout: None,
            macros: None,
        },
        tunables: mock_tunables(),
        dirs: mock_dirs(),
        layout: Default::default(),
        macros: HashMap::default(),
    }
}

pub async fn mock_store() -> ProgramStore {
    let (tx, _) = unbounded_channel();
    let homeserver = Url::parse("https://localhost").unwrap();
    let client = matrix_sdk::Client::new(homeserver).await.unwrap();
    let worker = Requester { tx, client, store: Weak::new() };

    let mut store = ChatStore::new(worker, mock_settings());

    // Add presence information.
    store.presences.get_or_default(TEST_USER1.clone());
    store.presences.get_or_default(TEST_USER2.clone());
    store.presences.get_or_default(TEST_USER3.clone());
    store.presences.get_or_default(TEST_USER4.clone());
    store.presences.get_or_default(TEST_USER5.clone());

    // let room_id = TEST_ROOM1_ID.clone();
    // let info = mock_room();
    //
    // store.rooms.insert(room_id.clone(), info);
    // store.names.insert(TEST_ROOM1_ALIAS.to_string(), room_id);
    todo!()
    // ProgramStore::new(store)
}
