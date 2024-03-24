# NAME

config.json – configuration file for iamb

# SYNOPSIS

Configuration must be placed under _~/.config/iamb/_ and is named config.json.

Example configuration usually comes bundled with your installation and can
typically be found in _/usr/share/iamb_.

As implied by the filename, the configuration is formatted in JSON. It's
structure and fields are described below.

# BASIC SETTINGS

These options are primitives at the top-level of the file.

**default_profile** (type: string)
> The default profile to connect to, unless overwritten by a commandline
> switch. It has to be defined in the *PROFILES* section.

# PROFILES

These options are configured as a map under the profiles name.

**user_id** (type: string)
> The user ID to use when connecting to the server. For example "user" for
> "@user:matrix.org".

**url** (type: string)
> The URL of the users server. For example "https://matrix.org" for
> "@user:matrix.org".

**settings** (type: settings object)
> Overwrite general settings for this account. The fields are identical to
> those in *TUNABLES*.

**layout** (type: startup layout object)
> Overwrite general settings for this account. The fields are identical to
> those in *STARTUP LAYOUT*.

**dirs** (type: XDG overrides object)
> Overwrite general settings for this account. The fields are identical to
> those in *DIRECTORIES*.

# TUNABLES

These options are configured as a map under the *settings* key and can be
overridden as described in *PROFILES*.

**log_level** (type: string)
> Specifies the lowest log level that should be shown. Possible values
> are: _trace_, _debug_, _info_, _warn_, and _error_.

**message_shortcode_display** (type: boolean)
> Defines whether or not emoji characters in messages should be replaced by
> their respective shortcodes.

**reaction_display** (type: boolean)
> Defines whether or not reactions should be shown.

**reaction_shortcode_display** (type: boolean)
> Defines whether or not reactions should be shown as their respective
> shortcode.

**read_receipt_send** (type: boolean)
> Defines whether or not read confirmations are sent.

**read_receipt_display** (type: boolean)
> Defines whether or not read confirmations are displayed.

**request_timeout** (type: uint64)
> Defines the maximum time per request in seconds.

**typing_notice_send** (type: boolean)
> Defines whether or not the typing state is sent.

**typing_notice_display** (type: boolean)
> Defines whether or not the typing state is displayed.

**user** (type: map)
> Overrides values for the specified user. See *USER OVERRIDES* for
> details on the format.

**default_room** (type: string)
> The room to show by default instead of a welcome-screen.

**message_user_color** (type: boolean)
> Defines whether or not the message body is colored like the username.

**notifications** (type: notifications object)
> Configures push-notifications, which are delivered as desktop 
> notifications if available.
> *enabled* `true` to enable the feature, defaults to `false`.
> *via* `"desktop"` to use desktop mechanism, or `"bell"` to use terminal bell.
> *show_message* to show the message in the desktop notification. Defaults
> to `true`. Messages are truncated beyond a small length.
> The notification _rules_ are stored server side, loaded once at startup,
> and are currently not configurable in iamb. In other words, you can
> simply change the rules with another client.

**image_preview** (type: image_preview object)
> Enable image previews and configure it. An empty object will enable the
> feature with default settings, omitting it will disable the feature.
> *size* is an optional object with *width* and *height* numbers, which are
> used to set the preview size in characters. Defaults to 66 and 10.
> *protocol* is an optional object to override settings that should normally
> be guessed automatically.
> *protocol.type* is an optional string with one of the protocol types:
> _sixel_, _kitty_, _halfblocks_.
> *protocol.font_size* is an optional list of two numbers representing font
> width and height in pixels.

**user_gutter_width** (type: usize)
> Specify the width of the column where usernames are displayed in a room.
> Usernames that are too long are truncated.

## USER OVERRIDES

Overrides are mapped onto matrix User IDs such as _@user:matrix.org_ and are
maps containing the following key value pairs.

**name** (type: string)
> Change the display name of the user.

**color** (type: string)
> Change the color the user is shown as. Possible values are: _black_,
> _blue_, _cyan_, _dark-gray_, _gray_, _green_, _light-blue_,
> _light-cyan_, _light-green_, _light-magenta_, _light-red_,
> _light-yellow_, _magenta_, _none_, _red_, _white_, _yellow_

# STARTUP LAYOUT

Specifies what initial set of tabs and windows to show when starting the
client. Configured as an object under the key *layout*.

**style** (type: string)
> Specifies what window layout to load when starting. Valid values are
> _restore_ to restore the layout from the last time the client was exited,
> _new_ to open a single window (uses the value of _default\_room_ if set), or
> _config_ to open the layout described under _tabs_.

**tabs** (type: array of window objects)
> If **style** is set to _config_, then this value will be used to open a set
> of tabs and windows at startup. Each object can contain either a **window**
> key specifying a username, room identifier or room alias to show, or a
> **split** key specifying an array of window objects.

# DIRECTORIES

Specifies the directories to save data in. Configured as a map under the key
*dirs*.

**cache** (type: string)
> Specifies where to store assets and temporary data in.

**logs** (type: string)
> Specifies where to store log files.

**downloads** (type: string)
> Specifies where to store downloaded files.

**image_previews** (type: string)
> Specifies where to store automatically downloaded image previews.

# SEE ALSO

*iamb(1)*

Full documentation is available online at \<https://iamb.chat\>
