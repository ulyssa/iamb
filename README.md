# iamb

## About

`iamb` is a Matrix client for the terminal that uses Vim keybindings.

This project is a work-in-progress, and there's still a lot to be implemented,
but much of the basic client functionality is already present.

## Installation

Install Rust and Cargo, and then run:

```
cargo install iamb
```

## Configuration

You can create a basic configuration in `$CONFIG_DIR/iamb/config.json` that looks like:

```json
{
    "profiles": {
        "example.com": {
            "url": "https://example.com",
            "@user:example.com"
        }
    }
}
```

## License

iamb is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/ulyssa/iamb/blob/master/LICENSE
