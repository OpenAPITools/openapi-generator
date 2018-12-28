
---
id: generator-opts-client-dart
title: Config Options for dart
sidebar_label: dart
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|browserClient|Is the client browser based| |null|
|pubName|Name in generated pubspec| |null|
|pubVersion|Version in generated pubspec| |null|
|pubDescription|Description in generated pubspec| |null|
|useEnumExtension|Allow the 'x-enum-values' extension for enums| |null|
|sourceFolder|source folder for generated code| |null|
|supportDart2|support dart2| |true|
