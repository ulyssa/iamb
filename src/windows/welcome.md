# Welcome to iamb!

## Useful Keybindings

- `<Enter>` will send a typed message
- `^V^J` can be used in Insert mode to enter a newline without submitting
- `O`/`o` can be used to insert blank lines before and after the cursor line
- `^Wm` can be used to toggle whether the message bar or scrollback is selected
- `^Wz` can be used to toggle whether the current window takes up the full screen

## Room Commands

- `:dms` will open a list of direct messages
- `:rooms` will open a list of joined rooms
- `:members` will open a list of members for the currently focused room or space
- `:spaces` will open a list of joined spaces
- `:join` can be used to switch to join a new room or start a direct message
- `:split` and `:vsplit` can be used to open rooms in a new window

## Verification Commands

The `:verify` command has several different subcommands for working with
verification requests. When used without any arguments, it will take you to a
list of current verifications, where you can see and compare the Emoji.

The different subcommands are:

- `:verify request USERNAME` will send a verification request to a user
- `:verify confirm USERNAME/DEVICE` will confirm a verification
- `:verify mismatch USERNAME/DEVICE` will cancel a verification where the Emoji don't match
- `:verify cancel USERNAME/DEVICE` will cancel a verification

## Other Useful Commands

- `:welcome` will take you back to this screen

## Additional Configuration

You can customize iamb in your `$CONFIG_DIR/iamb/config.json` file, where
`$CONFIG_DIR` is your system's per-user configuration directory.

You can edit the following values in the file:

- `"default_profile"`, a profile name to use when starting iamb if one wasn't specified
- `"cache"`, a directory for cached iamb
