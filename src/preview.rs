use std::{
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
};

use matrix_sdk::{
    media::{MediaFormat, MediaRequestParameters},
    ruma::{
        events::{
            room::{
                message::{MessageType, RoomMessageEventContent},
                MediaSource,
            },
            MessageLikeEvent,
        },
        OwnedEventId,
        OwnedRoomId,
    },
    Media,
};
use ratatui::layout::Rect;
use ratatui_image::Resize;

use crate::{
    base::{AsyncProgramStore, ChatStore, IambError},
    config::ImagePreviewSize,
    message::ImageStatus,
};

pub fn source_from_event(
    ev: &MessageLikeEvent<RoomMessageEventContent>,
) -> Option<(OwnedEventId, MediaSource)> {
    if let MessageLikeEvent::Original(ev) = &ev {
        if let MessageType::Image(c) = &ev.content.msgtype {
            return Some((ev.event_id.clone(), c.source.clone()));
        }
    }
    None
}

impl From<ImagePreviewSize> for Rect {
    fn from(value: ImagePreviewSize) -> Self {
        Rect::new(0, 0, value.width as _, value.height as _)
    }
}
impl From<Rect> for ImagePreviewSize {
    fn from(rect: Rect) -> Self {
        ImagePreviewSize { width: rect.width as _, height: rect.height as _ }
    }
}

/// Download and prepare the preview, and then lock the store to insert it.
pub fn spawn_insert_preview(
    store: AsyncProgramStore,
    room_id: OwnedRoomId,
    event_id: OwnedEventId,
    source: MediaSource,
    media: Media,
    cache_dir: PathBuf,
    preview: ImagePreviewSize,
) {
    tokio::spawn(async move {
        let img = download_or_load(event_id.to_owned(), source, media, cache_dir)
            .await
            .map(std::io::Cursor::new)
            .map(image::ImageReader::new)
            .map_err(IambError::Matrix)
            .and_then(|reader| reader.with_guessed_format().map_err(IambError::IOError))
            .and_then(|reader| reader.decode().map_err(IambError::Image));

        match img {
            Err(err) => {
                try_set_msg_preview_error(
                    &mut store.lock().await.application,
                    room_id,
                    event_id,
                    err,
                );
            },
            Ok(img) => {
                let mut locked = store.lock().await;

                match locked
                    .application
                    .rooms
                    .get_or_default(room_id.clone())
                    .get_event_mut(&event_id)
                    .ok_or_else(|| IambError::Preview("Message not found".to_string()))
                {
                    Err(err) => {
                        try_set_msg_preview_error(&mut locked.application, room_id, event_id, err);
                    },
                    Ok(msg) => msg.image_preview = ImageStatus::Loading(Some(img), preview),
                }
            },
        }
    });
}

pub async fn render_preview(
    store: AsyncProgramStore,
    room_id: OwnedRoomId,
    event_id: OwnedEventId,
) {
    let mut locked = store.lock().await;

    let picker = locked.application.picker.clone();
    let Some(img) = locked
        .application
        .rooms
        .get_or_default(room_id.clone())
        .get_event_mut(&event_id)
        .ok_or_else(|| IambError::Preview("Picker is empty".to_string()))
        .map(|msg| {
            if let ImageStatus::Loading(image, size) = &mut msg.image_preview {
                image.take().map(|image| (image, size.clone()))
            } else {
                None
            }
        })
        .transpose()
    else {
        // ignore if image is already being loaded
        return;
    };

    // make shure to unlock store while computing `new_protocol`
    drop(locked);

    let image = picker
        .ok_or_else(|| IambError::Preview("Picker is empty".to_string()))
        .and_then(|picker| {
            let (image, size) = img?;
            Ok((picker, image, size))
        })
        .and_then(|(picker, image, size)| {
            picker
                .new_protocol(image, size.into(), Resize::Fit(None))
                .map_err(|err| IambError::Preview(format!("{err}")))
        });

    let mut locked = store.lock().await;

    match image {
        Err(err) => {
            try_set_msg_preview_error(&mut locked.application, room_id, event_id, err);
        },
        Ok(backend) => {
            locked
                .application
                .rooms
                .get_or_default(room_id)
                .get_event_mut(&event_id)
                .unwrap()
                .image_preview = ImageStatus::Loaded(backend);
        },
    }
}

fn try_set_msg_preview_error(
    application: &mut ChatStore,
    room_id: OwnedRoomId,
    event_id: OwnedEventId,
    err: IambError,
) {
    let rooms = &mut application.rooms;

    match rooms
        .get_or_default(room_id.clone())
        .get_event_mut(&event_id)
        .ok_or_else(|| IambError::Preview("Message not found".to_string()))
    {
        Ok(msg) => msg.image_preview = ImageStatus::Error(format!("{err:?}")),
        Err(err) => {
            tracing::error!(
                "Failed to set error on msg.image_backend for event {}, room {}: {}",
                event_id,
                room_id,
                err
            )
        },
    }
}

async fn download_or_load(
    event_id: OwnedEventId,
    source: MediaSource,
    media: Media,
    mut cache_path: PathBuf,
) -> Result<Vec<u8>, matrix_sdk::Error> {
    cache_path.push(Path::new(event_id.localpart()));

    match File::open(&cache_path) {
        Ok(mut f) => {
            let mut buffer = Vec::new();
            f.read_to_end(&mut buffer)?;
            Ok(buffer)
        },
        Err(_) => {
            media
                .get_media_content(
                    &MediaRequestParameters { source, format: MediaFormat::File },
                    true,
                )
                .await
                .and_then(|buffer| {
                    if let Err(err) =
                        File::create(&cache_path).and_then(|mut f| f.write_all(&buffer))
                    {
                        return Err(err.into());
                    }
                    Ok(buffer)
                })
        },
    }
}
