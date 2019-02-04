
---
id: generator-opts-documentation-cwiki
title: Config Options for cwiki
sidebar_label: cwiki
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|appName|short name of the application| |null|
|appDescription|description of the application| |null|
|infoUrl|a URL where users can get more information about the application| |null|
|infoEmail|an email address to contact for inquiries about the application| |null|
|licenseInfo|a short description of the license| |null|
|licenseUrl|a URL pointing to the full license| |null|
|invokerPackage|root package for generated code| |null|
|groupId|groupId in generated pom.xml| |null|
|artifactId|artifactId in generated pom.xml| |null|
|artifactVersion|artifact version in generated pom.xml| |null|
