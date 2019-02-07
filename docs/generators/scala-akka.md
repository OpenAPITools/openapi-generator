
---
id: generator-opts-client-scala-akka
title: Config Options for scala-akka
sidebar_label: scala-akka
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPackage|package for generated models| |null|
|apiPackage|package for generated api classes| |null|
|sourceFolder|source folder for generated code| |null|
|mainPackage|Top-level package name, which defines 'apiPackage', 'modelPackage', 'invokerPackage'| |org.openapitools.client|
