#!/usr/bin/env bash

###
# Provides completion assistance for openapi-generator-cli
# Install
# Mac:
#    brew install bash-completion
#    cp openapi-generator-cli-completion.bash `brew --prefix`/etc/bash_completion.d
# Linux: many distributions include this automatically. Search for your distro-specific instructions.
# When in doubt, try sourcing this file:
#    type complete && source openapi-generator-cli
#
# see http://tldp.org/LDP/abs/html/tabexpansion.html
###

_openapi_generator_cli_completions() {
  COMPREPLY=()
  local IFS=$' \t\n'
  local options=()

  options+=("$($1 completion ${COMP_WORDS[@]:1})")

  case "${COMP_WORDS[1]}" in
    generate)
      case "${COMP_WORDS[@]:2}" in
        -l|--lang|-g|--generator-name)
        # TODO: This is a nice-to-have and not required.
        # Apply generator-specific options to additional properties. These can be queried via:
        # openapi-generator-cli config-help -l YOUR_LANG | grep '^\t' | grep -v '^\t\s\s\s\s' | tr -d '\t'
        # where YOUR_LANG would need to be evaluated as the value after the current switch.
        # but rather than switching on 'generate' maybe switch on --additional-properties?
        ;;
      esac
    ;;
    *)
    # ignore
    ;;
  esac

  # printf '%s\n' "${options[@]}"
  if [[ -n "${options[@]}" ]]; then
    COMPREPLY=( $(compgen  -W "${options}" -- ${2}) )
  fi
}

for i in openapi-generator-cli openapi-generator; do
  type $i > /dev/null 2>&1 && complete -F _openapi_generator_cli_completions $i
done
