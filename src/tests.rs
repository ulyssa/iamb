use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::sync::mpsc::sync_channel;

use matrix_sdk::ruma::{
    event_id,
    events::room::message::RoomMessageEventContent,
    server_name,
    user_id,
    EventId,
    OwnedRoomId,
    OwnedUserId,
    RoomId,
    UInt,
};

use lazy_static::lazy_static;
use modalkit::tui::style::Color;
use url::Url;

use crate::{
    base::{ChatStore, ProgramStore, RoomFetchStatus, RoomInfo},
    config::{
        ApplicationSettings,
        DirectoryValues,
        ProfileConfig,
        TunableValues,
        UserColor,
        UserDisplayTunables,
    },
    message::{
        Message,
        MessageContent,
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
    pub static ref MSG1_KEY: MessageKey = (LocalEcho, EventId::new(server_name!("example.com")));
    pub static ref MSG2_KEY: MessageKey =
        (OriginServer(UInt::new(1).unwrap()), EventId::new(server_name!("example.com")));
    pub static ref MSG3_KEY: MessageKey = (
        OriginServer(UInt::new(2).unwrap()),
        event_id!("$5jRz3KfVhaUzXtVj7k:example.com").to_owned()
    );
    pub static ref MSG4_KEY: MessageKey = (
        OriginServer(UInt::new(2).unwrap()),
        event_id!("$JP6qFV7WyXk5ZnexM3:example.com").to_owned()
    );
    pub static ref MSG5_KEY: MessageKey =
        (OriginServer(UInt::new(8).unwrap()), EventId::new(server_name!("example.com")));
}

pub fn mock_message1() -> Message {
    let content = RoomMessageEventContent::text_plain("writhe");
    let content = MessageContent::Original(content.into());

    Message::new(content, TEST_USER1.clone(), MSG1_KEY.0)
}

pub fn mock_message2() -> Message {
    let content = RoomMessageEventContent::text_plain("helium");
    let content = MessageContent::Original(content.into());

    Message::new(content, TEST_USER2.clone(), MSG2_KEY.0)
}

pub fn mock_message3() -> Message {
    let content = RoomMessageEventContent::text_plain("this\nis\na\nmultiline\nmessage");
    let content = MessageContent::Original(content.into());

    Message::new(content, TEST_USER2.clone(), MSG3_KEY.0)
}

pub fn mock_message4() -> Message {
    let content = RoomMessageEventContent::text_plain("help");
    let content = MessageContent::Original(content.into());

    Message::new(content, TEST_USER1.clone(), MSG4_KEY.0)
}

pub fn mock_message5() -> Message {
    let content = RoomMessageEventContent::text_plain("character");
    let content = MessageContent::Original(content.into());

    Message::new(content, TEST_USER2.clone(), MSG5_KEY.0)
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
        messages: mock_messages(),
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
        typing_notice: true,
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

pub fn mock_store() -> ProgramStore {
    let (tx, _) = sync_channel(5);
    let worker = Requester { tx };

    let mut store = ChatStore::new(worker, mock_settings());
    let room_id = TEST_ROOM1_ID.clone();
    let info = mock_room();

    store.rooms.insert(room_id, info);

    ProgramStore::new(store)
}
