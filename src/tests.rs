use std::collections::{BTreeMap, HashMap};
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
use modalkit::tui::style::{Color, Style};
use tokio::sync::mpsc::unbounded_channel;
use url::Url;

use crate::{
    base::{ChatStore, EventLocation, ProgramStore, RoomFetchStatus, RoomInfo},
    config::{
        user_color,
        user_style_from_color,
        ApplicationSettings,
        DirectoryValues,
        ProfileConfig,
        TunableValues,
        UserColor,
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

    keys.insert(MSG1_EVID.clone(), EventLocation::Message(MSG1_KEY.clone()));
    keys.insert(MSG2_EVID.clone(), EventLocation::Message(MSG2_KEY.clone()));
    keys.insert(MSG3_EVID.clone(), EventLocation::Message(MSG3_KEY.clone()));
    keys.insert(MSG4_EVID.clone(), EventLocation::Message(MSG4_KEY.clone()));
    keys.insert(MSG5_EVID.clone(), EventLocation::Message(MSG5_KEY.clone()));

    keys
}

pub fn mock_messages() -> Messages {
    let mut messages = BTreeMap::new();

    messages.insert(MSG1_KEY.clone(), mock_message1());
    messages.insert(MSG2_KEY.clone(), mock_message2());
    messages.insert(MSG3_KEY.clone(), mock_message3());
    messages.insert(MSG4_KEY.clone(), mock_message4());
    messages.insert(MSG5_KEY.clone(), mock_message5());

    messages
}

pub fn mock_room() -> RoomInfo {
    RoomInfo {
        name: Some("Watercooler Discussion".into()),
        tags: None,

        keys: mock_keys(),
        messages: mock_messages(),

        receipts: HashMap::new(),
        read_till: None,
        reactions: HashMap::new(),

        fetch_id: RoomFetchStatus::NotStarted,
        fetch_last: None,
        users_typing: None,
    }
}

pub fn mock_dirs() -> DirectoryValues {
    DirectoryValues {
        cache: PathBuf::new(),
        logs: PathBuf::new(),
        downloads: PathBuf::new(),
    }
}

pub fn mock_tunables() -> TunableValues {
    TunableValues {
        default_room: None,
        reaction_display: true,
        reaction_shortcode_display: false,
        read_receipt_send: true,
        read_receipt_display: true,
        typing_notice_send: true,
        typing_notice_display: true,
        users: vec![(TEST_USER5.clone(), UserDisplayTunables {
            color: Some(UserColor(Color::Black)),
            name: Some("USER 5".into()),
        })]
        .into_iter()
        .collect::<HashMap<_, _>>(),
    }
}

pub fn mock_settings() -> ApplicationSettings {
    ApplicationSettings {
        matrix_dir: PathBuf::new(),
        session_json: PathBuf::new(),
        profile_name: "test".into(),
        profile: ProfileConfig {
            user_id: user_id!("@user:example.com").to_owned(),
            url: Url::parse("https://example.com").unwrap(),
            settings: None,
            dirs: None,
        },
        tunables: mock_tunables(),
        dirs: mock_dirs(),
    }
}

pub async fn mock_store() -> ProgramStore {
    let (tx, _) = unbounded_channel();
    let homeserver = Url::parse("https://localhost").unwrap();
    let client = matrix_sdk::Client::new(homeserver).await.unwrap();
    let worker = Requester { tx, client };

    let mut store = ChatStore::new(worker, mock_settings());
    let room_id = TEST_ROOM1_ID.clone();
    let info = mock_room();

    store.rooms.insert(room_id, info);

    ProgramStore::new(store)
}
