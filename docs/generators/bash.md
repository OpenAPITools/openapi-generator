
---
id: generator-opts-client-bash
title: Config Options for bash
sidebar_label: bash
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|curlOptions|Default cURL options| |null|
|processMarkdown|Convert all Markdown Markup into terminal formatting| |false|
|scriptName|The name of the script that will be generated (e.g. petstore-cli)| |null|
|generateBashCompletion|Whether to generate the Bash completion script| |false|
|generateZshCompletion|Whether to generate the Zsh completion script| |false|
|hostEnvironmentVariable|Name of environment variable where host can be defined (e.g. PETSTORE_HOST='http://api.openapitools.org:8080')| |null|
|basicAuthEnvironmentVariable|Name of environment variable where username and password can be defined (e.g. PETSTORE_CREDS='username:password')| |null|
|apiKeyAuthEnvironmentVariable|Name of environment variable where API key can be defined (e.g. PETSTORE_APIKEY='kjhasdGASDa5asdASD')| |false|
