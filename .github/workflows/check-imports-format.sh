#!/bin/sh

while [ $# -gt 0 ]; do
  # check that all imports for a `use` statement come from the same module
  import_error=$(sed -n "
    # only consider use statements with braces
    /^ *use .*{/ {
      # load the import until the first closing brace into the pattern space
      :find-close
      /^[^}]*$/ {
        N
        b find-close
      }
      
      # show error if multiple braces exist
      /{.*{/ {
        i\\
invalid import in '$1' line:
        =
        i\\
containing multiple braces in one import:
        p
        b
      }

      # show error if qualified paths exist inside braces
      /{.*::/ {
        i\\
invalid import in '$1' line:
        =
        i\\
containing a qualified import inside braces:
        p
        b
      }
    }
    " "$1")
  if [ -n "$import_error" ]; then
    printf "%s\n" "$import_error"
    exit 1
  fi

  # check that imports are sorted as `std`, `other imports`, `crate` each seperated by a blank line
  import_pattern=$(sed -n "
    # include blank lines
    /^$/ {
      p
      d
    }
    # include std imports
    /^use std::/ {
      i\\
use std
      d
    }
    # include crate imports
    /^use crate::/ {
      i\\
use crate
      d
    }
    # include other imports without the target
    s/^use .*/use/p
  " "$1" | uniq | tr "\n" "_") # remove duplicates; we only care for the order of the imports; also grep doesn't play nice with line breaks

    # check that the pattern if followed
  printf "%s" "$import_pattern" | grep -q "^_\?\(use std__\)\?\(use__\)\?\(use crate__\)\?_\?$"
  if [ $? -ne 0 ]; then
    printf "imports in '%s' dont follow the pattern:
\`\`\`
use std::*;

use *;

use crate::*;

\`\`\`

instead is has the pattern:
\`\`\`
%s
\`\`\`
" "$1" "$import_pattern" | tr "_" "\n" >&2
    exit 1
  fi
 

  shift
done
