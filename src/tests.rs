use std::path::PathBuf;
use std::{collections::HashMap, iter::FromIterator as _};

use matrix_sdk::ruma::{
    EventId,
    OwnedEventId,
    OwnedRoomId,
    OwnedUserId,
    RoomId,
    UInt,
    event_id,
    events::room::message::RoomMessageEventContent,
    server_name,
    user_id,
};
use matrix_sdk::ruma::{MilliSecondsSinceUnixEpoch, assign};

use lazy_static::lazy_static;
use ratatui::style::{Color, Style};
use serde_json::{Map, Value};
use tokio::sync::mpsc::unbounded_channel;

use crate::message::MessageTimeStamp;
use crate::{
    base::{ChatStore, EventLocation, ProgramStore, RoomInfo},
    config::*,
    message::{Message, MessageEvent, MessageKey, Messages},
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
    pub static ref MSG1_EVID: OwnedEventId = EventId::new_v1(server_name!("example.com"));
    pub static ref MSG2_EVID: OwnedEventId = EventId::new_v1(server_name!("example.com"));
    pub static ref MSG3_EVID: OwnedEventId =
        event_id!("$5jRz3KfVhaUzXtVj7k:example.com").to_owned();
    pub static ref MSG4_EVID: OwnedEventId =
        event_id!("$JP6qFV7WyXk5ZnexM3:example.com").to_owned();
    pub static ref MSG5_EVID: OwnedEventId = EventId::new_v1(server_name!("example.com"));
    pub static ref MSG1_KEY: MessageKey = MessageKey {
        // 2000-01-01T00:00:00
        ts: MessageTimeStamp(MilliSecondsSinceUnixEpoch(UInt::new(946681200).unwrap())),
        id: MSG1_EVID.clone().into()
    };
    pub static ref MSG2_KEY: MessageKey = MessageKey {
        ts: MessageTimeStamp(MilliSecondsSinceUnixEpoch(UInt::new(1).unwrap())),
        id: MSG2_EVID.clone().into()
    };
    pub static ref MSG3_KEY: MessageKey = MessageKey {
        ts: MessageTimeStamp(MilliSecondsSinceUnixEpoch(UInt::new(2).unwrap())),
        id: MSG3_EVID.clone().into()
    };
    pub static ref MSG4_KEY: MessageKey = MessageKey {
        ts: MessageTimeStamp(MilliSecondsSinceUnixEpoch(UInt::new(2).unwrap())),
        id: MSG4_EVID.clone().into()
    };
    pub static ref MSG5_KEY: MessageKey = MessageKey {
        ts: MessageTimeStamp(MilliSecondsSinceUnixEpoch(UInt::new(8).unwrap())),
        id: MSG5_EVID.clone().into()
    };
}

pub fn user_style(user: &str) -> Style {
    user_style_from_color(user_color(user))
}

pub fn mock_room1_message(
    content: RoomMessageEventContent,
    sender: OwnedUserId,
    key: MessageKey,
) -> Message {
    let timestamp = key.ts.0;
    let event_id = key.id.as_origin().unwrap();

    let event = serde_json::from_value(Value::Object(Map::from_iter([
        ("type".to_owned(), Value::String("m.room.message".into())),
        ("content".to_owned(), serde_json::to_value(&content).unwrap()),
        ("event_id".to_owned(), serde_json::to_value(event_id).unwrap()),
        ("sender".to_owned(), serde_json::to_value(&sender).unwrap()),
        ("origin_server_ts".to_owned(), serde_json::to_value(timestamp).unwrap()),
        ("room_id".to_owned(), serde_json::to_value(&*TEST_ROOM1_ID).unwrap()),
    ])))
    .unwrap();

    Message::new(MessageEvent::Original(event, Default::default()), sender, timestamp.into())
}

pub fn mock_message1() -> Message {
    let content = RoomMessageEventContent::text_plain("writhe");

    mock_room1_message(content, TEST_USER1.clone(), MSG1_KEY.clone())
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

pub fn mock_keys() -> HashMap<OwnedEventId, EventLocation> {
    let mut keys = HashMap::new();

    keys.insert(MSG1_EVID.clone(), EventLocation::Message(None, MSG2_KEY.clone()));
    keys.insert(MSG2_EVID.clone(), EventLocation::Message(None, MSG2_KEY.clone()));
    keys.insert(MSG3_EVID.clone(), EventLocation::Message(None, MSG3_KEY.clone()));
    keys.insert(MSG4_EVID.clone(), EventLocation::Message(None, MSG4_KEY.clone()));
    keys.insert(MSG5_EVID.clone(), EventLocation::Message(None, MSG5_KEY.clone()));

    keys
}

pub fn mock_messages() -> Messages {
    let mut messages = Messages::main();

    messages.insert(MSG1_KEY.clone(), mock_message1());
    messages.insert(MSG2_KEY.clone(), mock_message2());
    messages.insert(MSG3_KEY.clone(), mock_message3());
    messages.insert(MSG4_KEY.clone(), mock_message4());
    messages.insert(MSG5_KEY.clone(), mock_message5());

    messages
}

pub fn mock_room() -> RoomInfo {
    let mut room = RoomInfo::default();
    room.name = Some("Watercooler Discussion".into());
    room.keys = mock_keys();
    *room.get_thread_mut(None) = mock_messages();
    room
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
        default_markup: Default::default(),
        ignorecase: false,
        default_room: None,
        encryption: Encryption::default().values(),
        input_prompt: None,
        log_level: "warn".into(),
        max_log_files: 7,
        message_shortcode_display: false,
        normal_after_send: true,
        proxy: Proxy::default().values(),
        reaction_display: true,
        reaction_shortcode_display: false,
        read_receipt_send: true,
        read_receipt_trigger: Default::default(),
        read_receipt_display: true,
        request_timeout: 120,
        sort: SortOverrides::default().values(),
        state_event_display: true,
        terminal: Terminal::default().values(),
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
        image_preview: assign!(ImagePreview::default().values(), {enabled: false}),
        user_gutter_width: 30,
        tabstop: 4,
        members_split: Default::default(),
        default_split: Default::default(),
        ssl_verify: true,
        cache_policy: Default::default(),
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
            password_file: None,
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
    let client = matrix_sdk::Client::builder()
        .homeserver_url("https://localhost")
        // don't panic if no certs are available like in a nix build sandbox
        .disable_ssl_verification()
        .build()
        .await
        .unwrap();
    let worker = Requester { tx, client };

    let mut store = ChatStore::new(worker, mock_settings());

    // Add presence information.
    store.presences.get_or_default(TEST_USER1.clone());
    store.presences.get_or_default(TEST_USER2.clone());
    store.presences.get_or_default(TEST_USER3.clone());
    store.presences.get_or_default(TEST_USER4.clone());
    store.presences.get_or_default(TEST_USER5.clone());

    let room_id = TEST_ROOM1_ID.clone();
    let info = mock_room();

    store.rooms.insert(room_id.clone(), info);
    store.names.insert(TEST_ROOM1_ALIAS.to_string(), room_id);

    ProgramStore::new(store)
}
