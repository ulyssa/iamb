use std::error::Error;
use std::fs;
use std::iter::FromIterator;
use std::path::PathBuf;

use mandown::convert;
use vergen::EmitBuilder;

const IAMB_1_MD: &str = include_str!("docs/iamb.1.md");
const IAMB_5_MD: &str = include_str!("docs/iamb.5.md");

fn main() -> Result<(), Box<dyn Error>> {
    EmitBuilder::builder().git_sha(true).emit()?;

    // Build the manual pages.
    println!("cargo:rerun-if-changed=docs/iamb.1.md");
    println!("cargo:rerun-if-changed=docs/iamb.5.md");

    let iamb_1 = convert(IAMB_1_MD, "IAMB", 1);
    let iamb_5 = convert(IAMB_5_MD, "IAMB", 5);

    let out_dir = std::env::var("OUT_DIR");
    let out_dir = out_dir.as_deref().unwrap_or("docs");

    fs::write(PathBuf::from_iter([out_dir, "iamb.1"]), iamb_1.as_bytes())?;
    fs::write(PathBuf::from_iter([out_dir, "iamb.5"]), iamb_5.as_bytes())?;

    Ok(())
}
