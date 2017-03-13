# Bash script generator for Swagger Codegen

## Overview
This is a Bash client script codegen.

The codegen creates a standalone, single-file Bash script client to quickly test and access Swagger annotated REST services. The generated script uses underneath [cURL](https://curl.haxx.se) to make actual REST calls.

The generated Bash script has only 2 dependencies:
- Bash (>= 4.3)
- cURL

## Features
- Fully automatic generation of a client Bash script to access any Swagger-defined REST service
- Generation of Bash and Zsh completion scripts
- All valid cURL options can be passed directly
- Preview of cURL commands to execute each operation using `--dry-run` option
- Complete help for entire service as well as for each operation
- No external dependencies besides Bash and cURL

## Usage

### Generating Bash client for REST service

Get the sources:
```shell
$ git clone https://github.com/swagger-api/swagger-codegen
```

Build the codegen:
```shell
$ mvn package
```

Define custom codegen properties in a Json file, e.g.:
```shell
{
  "processMarkdown": true,
  "curlOptions": "-sS --tlsv1.2",
  "scriptName": "petstore-cli",
  "generateBashCompletion": true,
  "generateZshCompletion": true,
  "hostEnvironmentVariable": "PETSTORE_HOST",
  "basicAuthEnvironmentVariable": "PETSTORE_BASIC_AUTH",
  "apiKeyAuthEnvironmentVariable": "PETSTORE_API_KEY"
}
```

Generate the client:
```shell
$ java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate -i http://petstore.swagger.io/v2/swagger.json -l bash -o generated/bash/petstore -c modules/swagger-codegen/src/test/resources/2_0/bash-config.json

$ chmod +x generated/bash/petstore/petstore-cli
```

Enjoy:
```shell
$ cd generated/bash/petstore
$ ./petstore-cli -h

Swagger Petstore command line client (API version 1.0.0)

Usage

  petstore-cli [-h|--help] [-V|--version] [--about] [<curl-options>]
           [-ac|--accept <mime-type>] [-ct,--content-type <mime-type>]
           [--host <url>] [--dry-run] <operation> [-h|--help] [<headers>]
           [<parameters>] [<body-parameters>]

  - <url> - endpoint of the REST service without basepath
           Can also be specified in PETSTORE_HOST environment variable.
  - <curl-options> - any valid cURL options can be passed before <operation>
  - <mime-type> - either full mime-type or one of supported abbreviations:
                   (text, html, md, csv, css, rtf, json, xml, yaml, js, bin,
                    rdf, jpg, png, gif, bmp, tiff)
  - <headers> - HTTP headers can be passed in the form HEADER:VALUE
  - <parameters> - REST operation parameters can be passed in the following
                   forms:
                   * KEY=VALUE - path or query parameters
  - <body-parameters> - simple JSON body content (first level only) can be build
                        using the following arguments:
                        * KEY==VALUE - body parameters which will be added to body
                                      JSON as '{ ..., "KEY": "VALUE", ... }'
                        * KEY:=VALUE - body parameters which will be added to body
                                      JSON as '{ ..., "KEY": VALUE, ... }'

Authentication methods

  - Api-key - add 'api_key:<api-key>' after <operation>
              or export PETSTORE_API_KEY='<api-key>'
  - OAuth2 (flow: implicit)
      Authorization URL:
        * http://petstore.swagger.io/oauth/dialog
      Scopes:
        * write:pets - modify pets in your account
        * read:pets - read your pets

Operations (grouped by tags)

[pet]
  addPet             Add a new pet to the store
  deletePet          Deletes a pet
  findPetsByStatus   Finds Pets by status
  findPetsByTags     Finds Pets by tags
  getPetById         Find pet by ID
  updatePet          Update an existing pet
  updatePetWithForm  Updates a pet in the store with form data
  uploadFile         uploads an image

[store]
  deleteOrder   Delete purchase order by ID
  getInventory  Returns pet inventories by status
  getOrderById  Find purchase order by ID
  placeOrder    Place an order for a pet

[user]
  createUser                 Create user
  createUsersWithArrayInput  Creates list of users with given input array
  createUsersWithListInput   Creates list of users with given input array
  deleteUser                 Delete user
  getUserByName              Get user by user name
  loginUser                  Logs user into the system
  logoutUser                 Logs out current logged in user session
  updateUser                 Updated user

Options
  -h,--help       Print this help
  -V,--version        Print API version
  --about       Print the information about service
  --host <url>        Specify the host URL
                      (e.g. 'https://petstore.swagger.io')
  --force       Force command invocation in spite of missing
                required parameters or wrong content type
  --dry-run       Print out the cURL command without
                  executing it
  -ac,--accept <mime-type>    Set the 'Accept' header in the request
  -ct,--content-type <mime-type>  Set the 'Content-type' header in
                                  the request
```

Client generator takes several specific configuration options:
* *processMarkdown* - [boolean] if set to `true`, all text (descriptions) in the Swagger specification will be treated as Markdown and converted to terminal formatting commands,
* *curlOptions* - [string] a list of default cURL options that will be added to each command
* *scriptName* - [string] the name of the target script, necessary when building Bash completion script
* *generateBashCompletion* - [boolean] if set to `true` the Bash completion script will be generated
* *generateZshCompletion* - [boolean] if set to `true` the Bash completion script will be generated
* *hostEnvironmentVariable* - [string] the name of environment variable to search for default host
* *basicAuthEnvironmentVariable* - [string] the name of environment variable to search for default basic auth credentials
* *apiKeyAuthEnvironmentVariable* - [string] the name of environment variable to search for default api key

These options can be specified in a Json file used when running the codegen using option `-c` (see [example](resources/example-config.json)).

### Using the generated Bash script

```shell
# Print the list of operations available on the service
$ petstore-cli --help

# Print the service description
$ petstore-cli --about

# Print detailed information about specific operation
$ petstore-cli addPet --help

# Call REST API operation
$ echo '{"id":891,"name":"lucky","status":"available"}' | petstore-cli --host http://petstore.swagger.io --content-type json addPet -

{"id":891,"name":"lucky","photoUrls":[],"tags":[],"status":"available"}

# The above is equivalent to
$ petstore-cli --host http://petstore.swagger.io --content-type json --accept xml addPet id:=891 name==lucky status==available

<xml version="1.0" encoding="UTF-8" standalone="yes"?><Pet><id>891</id><name>lucky</name><photoUrls/><status>available</status><tags/></Pet>


# Preview the cURL command without actually executing it
# The above is equivalent to
$ petstore-cli --host http://petstore.swagger.io --content-type json --dry-run addPet id:=891 name==lucky status==available

curl -sS --tlsv1.2 -H 'Content-type: application/json' -X POST -d '{"name": "lucky", "status": "available", "id": 891}' "http://petstore.swagger.io/v2/pet"
```

## Shell completion

### Bash
The generated bash-completion script can be either directly loaded to the current Bash session using:

```shell
source output/petstore-cli.bash-completion
```

Alternatively, the script can be copied to the `/etc/bash-completion.d` (or on OSX with Homebrew to `/usr/local/etc/bash-completion.d`):

```shell
sudo cp output/petstore-cli.bash-completion /etc/bash-completion.d/petstore-cli
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
In Zsh, the generated `_{{scriptName}}` file (e.g. _petstore-cli) must be copied to one of the folders under `$fpath` variable.


## TODO
- [ ] Add enum values for parameters shell completion
- [ ] Wrap handling of errors returned by the service, using comments defined in the Swagger specification
- [ ] Improve `--help` and `--about` formatting
- [ ] Add support to bash 4.0-4.2 (currently must be >= 4.3)
- [ ] Add manpage generation
- [ ] Add support for form data
- [ ] Move todos to Github issues
