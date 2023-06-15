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

**dirs** (type: XDG overrides object)
> Overwrite general settings for this account. The fields are identical to
> those in *DIRECTORIES*.

# TUNABLES

These options are configured as a map under the *settings* key and can be
overridden as described in *PROFILES*.

**log_level** (type: string)
> Specifies the lowest log level that should be shown. Possible values
> are: _trace_, _debug_, _info_, _warn_, and _error_.

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

# DIRECTORIES

Specifies the directories to save data in. Configured as a map under the key
*dirs*.

**cache** (type: string)
> Specifies where to store assets and temporary data in.

**logs** (type: string)
> Specifies where to store log files.

**downloads** (type: string)
> Specifies where to store downloaded files.

# SEE ALSO

*iamb(1)*

Full documentation is available online at \<https://iamb.chat\>
