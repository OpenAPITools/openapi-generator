# Swagger Petstore Bash client

## Overview
This is a Bash client script for accessing Swagger Petstore service.

The script uses  cURL underneath for making all REST calls.

## Usage

```shell
# Make sure the script has executable rights
$ chmod u+x petstore-cli

# Print the list of operations available on the service
$ ./petstore-cli -h

# Print the service description
$ ./petstore-cli --about

# Print detailed information about specific operation
$ ./petstore-cli <operationId> -h

# Make GET request 
./petstore-cli --host http://<hostname>:<port> --accept xml <operationId> <queryParam1>=<value1> <header_key1>:<header_value2>

# Make GET request using arbitrary curl options (must be passed before <operationId>) to an SSL service using username:password
petstore-cli -k -sS --tlsv1.2 --host https://<hostname> -u <user>:<password> --accept xml <operationId> <queryParam1>=<value1> <header_key1>:<header_value2>

# Make POST request
$ echo '<body_content>' | petstore-cli --host <hostname> --content-type json <operationId> -

# Make POST request with simple JSON content, e.g.:
# {
#   "key1": "value1",
#   "key2": "value2",
#   "key3": 23
# }
$ echo '<body_content>' | petstore-cli --host <hostname> --content-type json <operationId> key1==value1 key2=value2 key3:=23 -

# Preview the cURL command without actually executing it
$ petstore-cli --host http://<hostname>:<port> --dry-run <operationid>

```

## Shell completion

### Bash
The generated bash-completion script can be either directly loaded to the current Bash session using:

```shell
source petstore-cli.bash-completion
```

Alternatively, the script can be copied to the `/etc/bash-completion.d` (or on OSX with Homebrew to `/usr/local/etc/bash-completion.d`):

```shell
sudo cp petstore-cli.bash-completion /etc/bash-completion.d/petstore-cli
```

#### OS X
On OSX you might need to install bash-completion using Homebrew:
```shell
brew install bash-completion
```
and add the following to the `~/.bashrc`:

```shell
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
``` 

### Zsh
In Zsh, the generated `_petstore-cli` Zsh completion file must be copied to one of the folders under `$FPATH` variable.

