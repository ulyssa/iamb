use std::error::Error;

use vergen_gitcl::{Emitter, Gitcl};

fn main() -> Result<(), Box<dyn Error>> {
    let gitctl = Gitcl::builder().sha(true).build();
    Emitter::default().add_instructions(&gitctl)?.emit()?;

    Ok(())
}
