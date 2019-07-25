
---
id: generator-opts-client-apex
title: Config Options for apex
sidebar_label: apex
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|classPrefix|Prefix for generated classes. Set this to avoid overwriting existing classes in your org.| |null|
|apiVersion|The Metadata API version number to use for components in this package.| |null|
|buildMethod|The build method for this package.| |null|
|namedCredential|The named credential name for the HTTP callouts| |null|
