
---
id: generator-opts-client-elixir
title: Config Options for elixir
sidebar_label: elixir
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|invokerPackage|The main namespace to use for all classes. e.g. Yay.Pets| |null|
|licenseHeader|The license header to prepend to the top of all source files.| |null|
|packageName|Elixir package name (convention: lowercase).| |null|
