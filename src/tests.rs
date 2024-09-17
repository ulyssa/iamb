use std::collections::HashMap;
use std::path::PathBuf;

use matrix_sdk::ruma::{
    event_id,
    events::room::message::{OriginalRoomMessageEvent, RoomMessageEventContent},
    server_name,
    user_id,
    EventId,
    OwnedEventId,
    OwnedRoomId,
    OwnedUserId,
    RoomId,
    UInt,
};

use lazy_static::lazy_static;
use ratatui::style::{Color, Style};
use tokio::sync::mpsc::unbounded_channel;
use tracing::Level;
use url::Url;

use crate::{
    base::{ChatStore, EventLocation, ProgramStore, RoomInfo},
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
    message::{
        Message,
        MessageEvent,
        MessageKey,
        MessageTimeStamp::{LocalEcho, OriginServer},
        Messages,
    },
    worker::Requester,
};

const TEST_ROOM1_ALIAS: &str = "#room1:example.com";

lazy_static! {
    pub static ref TEST_ROOM1_ID: OwnedRoomId = RoomId::new(server_name!("example.com")).to_owned();
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
    pub static ref MSG1_KEY: MessageKey = (LocalEcho, MSG1_EVID.clone());
    pub static ref MSG2_KEY: MessageKey = (OriginServer(UInt::new(1).unwrap()), MSG2_EVID.clone());
    pub static ref MSG3_KEY: MessageKey = (OriginServer(UInt::new(2).unwrap()), MSG3_EVID.clone());
    pub static ref MSG4_KEY: MessageKey = (OriginServer(UInt::new(2).unwrap()), MSG4_EVID.clone());
    pub static ref MSG5_KEY: MessageKey = (OriginServer(UInt::new(8).unwrap()), MSG5_EVID.clone());
}

pub fn user_style(user: &str) -> Style {
    user_style_from_color(user_color(user))
}

pub fn mock_room1_message(
    content: RoomMessageEventContent,
    sender: OwnedUserId,
    key: MessageKey,
) -> Message {
    let origin_server_ts = key.0.as_millis().unwrap();
    let event_id = key.1;

    let event = OriginalRoomMessageEvent {
        content,
        event_id,
        sender,
        origin_server_ts,
        room_id: TEST_ROOM1_ID.clone(),
        unsigned: Default::default(),
    };

    event.into()
}

pub fn mock_message1() -> Message {
    let content = RoomMessageEventContent::text_plain("writhe");
    let content = MessageEvent::Local(MSG1_EVID.clone(), content.into());

    Message::new(content, TEST_USER1.clone(), MSG1_KEY.0)
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

    keys.insert(MSG1_EVID.clone(), EventLocation::Message(None, MSG1_KEY.clone()));
    keys.insert(MSG2_EVID.clone(), EventLocation::Message(None, MSG2_KEY.clone()));
    keys.insert(MSG3_EVID.clone(), EventLocation::Message(None, MSG3_KEY.clone()));
    keys.insert(MSG4_EVID.clone(), EventLocation::Message(None, MSG4_KEY.clone()));
    keys.insert(MSG5_EVID.clone(), EventLocation::Message(None, MSG5_KEY.clone()));

    keys
}

pub fn mock_messages() -> Messages {
    let mut messages = Messages::default();

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
        image_previews: PathBuf::new(),
    }
}

pub fn mock_tunables() -> TunableValues {
    TunableValues {
        default_room: None,
        log_level: Level::INFO,
        message_shortcode_display: false,
        reaction_display: true,
        reaction_shortcode_display: false,
        read_receipt_send: true,
        read_receipt_display: true,
        request_timeout: 120,
        sort: SortOverrides::default().values(),
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
        notifications: Notifications {
            enabled: false,
            via: NotifyVia::default(),
            show_message: true,
        },
        image_preview: None,
        user_gutter_width: 30,
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
