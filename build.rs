use std::error::Error;

use vergen_gitcl::{Emitter, GitclBuilder};

fn main() -> Result<(), Box<dyn Error>> {
    let gitctl = GitclBuilder::default().sha(true).build()?;
    Emitter::default().add_instructions(&gitctl)?.emit()?;

    Ok(())
}
