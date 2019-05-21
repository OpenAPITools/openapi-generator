
---
id: generator-opts-server-php-slim
title: Config Options for php-slim
sidebar_label: php-slim
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPackage|package for generated models| |null|
|apiPackage|package for generated api classes| |null|
|variableNamingConvention|naming convention of variable name, e.g. camelCase.| |camelCase|
|invokerPackage|The main namespace to use for all classes. e.g. Yay\Pets| |null|
|packageName|The main package name for classes. e.g. GeneratedPetstore| |null|
|srcBasePath|The directory to serve as source root.| |null|
|artifactVersion|The version to use in the composer package version field. e.g. 1.2.3| |null|
