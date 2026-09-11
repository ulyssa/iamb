//! Audio device selection for calls.
//!
//! LiveKit's platform audio device module owns the microphone and speaker, so
//! everything here is a thin, human-friendly layer over [`PlatformAudio`]:
//! turning its device lists into something printable, resolving what the user
//! typed at `:call device` into a device id, and remembering the choice.
//!
//! Only compiled when the `voip` feature is enabled.

use anyhow::{Result, anyhow};
use livekit::PlatformAudio;
use livekit::rtc_engine::lk_runtime::LkRuntime;
use livekit::webrtc::peer_connection_factory::native::PeerConnectionFactoryExt;
use serde::{Deserialize, Serialize};

/// Which end of the call a device sits on.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DeviceKind {
    /// The capture device.
    Microphone,

    /// The playback device.
    Speaker,
}

impl DeviceKind {
    /// The word the user types for this kind at `:call device`.
    pub fn keyword(&self) -> &'static str {
        match self {
            DeviceKind::Microphone => "mic",
            DeviceKind::Speaker => "speaker",
        }
    }

    /// The heading this kind gets in a device listing.
    pub fn heading(&self) -> &'static str {
        match self {
            DeviceKind::Microphone => "Microphones",
            DeviceKind::Speaker => "Speakers",
        }
    }
}

/// The devices the user has chosen, remembered between calls and between runs.
///
/// Devices are stored by name rather than by id: ids are opaque platform GUIDs,
/// and a name is what the user recognises if they ever open the file.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct DevicePreferences {
    /// The chosen microphone, or `None` to use the system default.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub microphone: Option<String>,

    /// The chosen speaker, or `None` to use the system default.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub speaker: Option<String>,
}

impl DevicePreferences {
    /// The remembered device of the given kind.
    pub fn get(&self, kind: DeviceKind) -> Option<&str> {
        let name = match kind {
            DeviceKind::Microphone => &self.microphone,
            DeviceKind::Speaker => &self.speaker,
        };

        name.as_deref()
    }

    /// Remember a device of the given kind.
    pub fn set(&mut self, kind: DeviceKind, name: String) {
        match kind {
            DeviceKind::Microphone => self.microphone = Some(name),
            DeviceKind::Speaker => self.speaker = Some(name),
        }
    }
}

/// One available audio device.
#[derive(Clone, Debug, Eq, PartialEq)]
struct Device {
    /// Position in the platform's device list, and the only handle the audio
    /// device module will actually act on. See [`switch`].
    index: usize,

    /// Human-readable name. Not necessarily unique: two identical USB
    /// microphones report the same name, so never use it to identify a device
    /// we have already picked out.
    name: String,
}

/// The devices of one kind.
fn enumerate(audio: &PlatformAudio, kind: DeviceKind) -> Vec<Device> {
    match kind {
        DeviceKind::Microphone => {
            audio
                .recording_devices()
                .map(|d| Device { index: d.index, name: d.name })
                .collect()
        },
        DeviceKind::Speaker => {
            audio
                .playout_devices()
                .map(|d| Device { index: d.index, name: d.name })
                .collect()
        },
    }
}

/// Render the available devices, marking the remembered ones.
///
/// The mark says which device iamb will *ask* for; LiveKit offers no way to read
/// back what the audio device module actually settled on, so an unmarked list
/// means we have never chosen and the system default is in use.
pub fn format_listing(audio: &PlatformAudio, prefs: &DevicePreferences) -> String {
    let mut out = String::new();

    for kind in [DeviceKind::Microphone, DeviceKind::Speaker] {
        out.push_str(kind.heading());
        out.push_str(":\n");

        let devices = enumerate(audio, kind);

        if devices.is_empty() {
            out.push_str("  (none found)\n");
            continue;
        }

        let chosen = prefs.get(kind);

        for device in devices {
            let mark = if Some(device.name.as_str()) == chosen {
                "  (selected)"
            } else {
                ""
            };

            out.push_str(&format!("  {}  {}{mark}\n", device.index, device.name));
        }
    }

    out.push_str("\nSelect with `:call device ");
    out.push_str(DeviceKind::Microphone.keyword());
    out.push_str(" <index|name>`.\n");

    out
}

/// Find the device the user meant.
///
/// A bare number picks by index; anything else matches a device name, exactly if
/// possible and otherwise as a case-insensitive substring so that `:call device
/// mic yeti` does the obvious thing.
fn resolve(audio: &PlatformAudio, kind: DeviceKind, spec: &str) -> Result<Device> {
    resolve_in(&enumerate(audio, kind), kind, spec)
}

/// The matching rules behind [`resolve`], over an already enumerated list.
fn resolve_in(devices: &[Device], kind: DeviceKind, spec: &str) -> Result<Device> {
    if let Ok(wanted) = spec.parse::<usize>() {
        return devices
            .iter()
            .find(|device| device.index == wanted)
            .cloned()
            .ok_or_else(|| anyhow!("no {} with index {wanted}", kind.keyword()));
    }

    if let Some(device) = devices.iter().find(|device| device.name == spec) {
        return Ok(device.clone());
    }

    let spec_lower = spec.to_lowercase();
    let mut matches = devices
        .iter()
        .filter(|device| device.name.to_lowercase().contains(&spec_lower));

    let Some(device) = matches.next() else {
        return Err(anyhow!("no {} matching {spec:?}", kind.keyword()));
    };

    if matches.next().is_some() {
        return Err(anyhow!("{spec:?} matches more than one {}", kind.keyword()));
    }

    Ok(device.clone())
}

/// Switch to a device by its position in the platform's device list,
/// hot-swapping it if a call is already running.
///
/// This goes to the audio device module directly rather than through
/// [`PlatformAudio::switch_recording_device`], because that API takes a device
/// GUID and on Linux the WebRTC ADM reports an *empty* GUID for every device.
/// The lookup behind it walks the device list for the first equal GUID, so an
/// empty one matches device 0 immediately, and it selects the default device and
/// returns success.
///
/// Indices are what the ADM actually keys on, and the same
/// [`PeerConnectionFactory`] exposes them. `audio` is not read, but holding it
/// is what guarantees the platform ADM these calls reach is still acquired.
///
/// [`PeerConnectionFactory`]: livekit::webrtc::peer_connection_factory::PeerConnectionFactory
fn switch(audio: &PlatformAudio, kind: DeviceKind, index: usize) -> Result<()> {
    let _ = audio;

    let index =
        u16::try_from(index).map_err(|_| anyhow!("device index {index} is out of range"))?;

    let runtime = LkRuntime::instance();
    let factory = runtime.pc_factory();

    // Capture and playback have to be stopped before the device underneath them
    // can change.
    match kind {
        DeviceKind::Microphone => {
            let running = factory.recording_is_initialized();

            if running && !factory.stop_recording() {
                return Err(anyhow!("could not stop capturing to change the microphone"));
            }

            if !factory.set_recording_device(index) {
                return Err(anyhow!("the audio device module rejected microphone {index}"));
            }

            if running && !(factory.init_recording() && factory.start_recording()) {
                return Err(anyhow!("could not start capturing from the new microphone"));
            }
        },
        DeviceKind::Speaker => {
            let running = factory.playout_is_initialized();

            if running && !factory.stop_playout() {
                return Err(anyhow!("could not stop playback to change the speaker"));
            }

            if !factory.set_playout_device(index) {
                return Err(anyhow!("the audio device module rejected speaker {index}"));
            }

            if running && !(factory.init_playout() && factory.start_playout()) {
                return Err(anyhow!("could not start playback on the new speaker"));
            }
        },
    }

    Ok(())
}

/// Resolve what the user typed, switch to that device, and return its full name.
pub fn select(audio: &PlatformAudio, kind: DeviceKind, spec: &str) -> Result<String> {
    let device = resolve(audio, kind, spec)?;

    switch(audio, kind, device.index)?;

    Ok(device.name)
}

/// Apply the remembered devices at the start of a call.
///
/// A device that is no longer plugged in is not an error: the call goes ahead on
/// the system default rather than refusing to start.
pub fn apply(audio: &PlatformAudio, prefs: &DevicePreferences) {
    for kind in [DeviceKind::Microphone, DeviceKind::Speaker] {
        let Some(name) = prefs.get(kind) else {
            continue;
        };

        // Preferences are stored by name, which is all the user gave us. If two
        // devices share a name we can only take the first.
        let Some(device) = enumerate(audio, kind).into_iter().find(|d| d.name == name) else {
            tracing::warn!("the remembered {} {name:?} is not available", kind.keyword());
            continue;
        };

        if let Err(e) = switch(audio, kind, device.index) {
            tracing::warn!("could not select the remembered {}: {e:#}", kind.keyword());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn device(index: usize, name: &str) -> Device {
        Device { index, name: name.to_string() }
    }

    fn devices() -> Vec<Device> {
        vec![
            device(0, "Built-in Audio Analog Stereo"),
            device(1, "Yeti Stereo Microphone"),
            device(2, "Yeti Nano"),
        ]
    }

    fn resolve(spec: &str) -> Result<Device> {
        resolve_in(&devices(), DeviceKind::Microphone, spec)
    }

    #[test]
    fn test_resolve_by_index() {
        assert_eq!(resolve("1").unwrap().name, "Yeti Stereo Microphone");
    }

    #[test]
    fn test_resolve_by_index_keeps_the_right_duplicate() {
        // Two identical microphones report the same name, so an index must
        // resolve to the device at that index and not to whichever one a later
        // name lookup would have found first.
        let devices = vec![device(0, "USB Microphone"), device(1, "USB Microphone")];

        let picked = resolve_in(&devices, DeviceKind::Microphone, "1").unwrap();

        assert_eq!(picked.index, 1);
    }

    #[test]
    fn test_resolve_index_out_of_range() {
        assert!(resolve("9").is_err());
    }

    #[test]
    fn test_resolve_exact_name_beats_substring() {
        // "Yeti Nano" is also a substring match for two entries when lowercased
        // to "yeti", but an exact hit must win outright.
        assert_eq!(resolve("Yeti Nano").unwrap().name, "Yeti Nano");
    }

    #[test]
    fn test_resolve_substring_is_case_insensitive() {
        assert_eq!(resolve("built-in").unwrap().name, "Built-in Audio Analog Stereo");
    }

    #[test]
    fn test_resolve_ambiguous_substring_is_an_error() {
        // "yeti" matches both Yeti devices, so we refuse rather than guess.
        assert!(resolve("yeti").is_err());
    }

    #[test]
    fn test_resolve_unknown_name() {
        assert!(resolve("nonexistent").is_err());
    }

    #[test]
    fn test_preferences_round_trip() {
        let mut prefs = DevicePreferences::default();
        assert_eq!(prefs.get(DeviceKind::Microphone), None);

        prefs.set(DeviceKind::Microphone, "Yeti Nano".to_string());
        assert_eq!(prefs.get(DeviceKind::Microphone), Some("Yeti Nano"));
        assert_eq!(prefs.get(DeviceKind::Speaker), None);

        let json = serde_json::to_string(&prefs).unwrap();
        assert_eq!(serde_json::from_str::<DevicePreferences>(&json).unwrap(), prefs);
    }
}
