
---
id: generator-opts-documentation-dynamic-html
title: Config Options for dynamic-html
sidebar_label: dynamic-html
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|invokerPackage|root package for generated code| |null|
|groupId|groupId in generated pom.xml| |null|
|artifactId|artifactId in generated pom.xml| |null|
|artifactVersion|artifact version in generated pom.xml| |null|
