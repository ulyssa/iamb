## Call Commands

The `:call` command joins the voice call in the currently focused room. It also
works on the room highlighted in a room, DM, space, member, or chat list, so you
can call a room without entering it first.

The different subcommands are:

- `:call` will join the call in the focused room
- `:call hangup` will leave the call
- `:call mute` and `:call unmute` will toggle your microphone
- `:call devices` will list the available microphones and speakers
- `:call device mic NAME|INDEX` will choose the microphone
- `:call device speaker NAME|INDEX` will choose the speaker

A device can be named by its index from `:call devices`, by its exact name, or
by any unambiguous part of one, so `:call device mic yeti` will work. Names do
not need quoting, and your choice is remembered between restarts.

The device subcommands work whether or not you are in a call, so you can set
things up before dialling.

While a call is running, the room shows a green banner listing who is in it,
with `▸` marking whoever is speaking. Rooms with a call in progress are also
marked with `📞` in the room lists.

