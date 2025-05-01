#!/bin/bash

# Function to provide completions for 'my_script'
_my_script_completions() {
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=($(compgen -W "option1 option2 option3" -- $cur))
}

# Register the completion function for 'my_script'
complete -F _my_script_completions my_script
