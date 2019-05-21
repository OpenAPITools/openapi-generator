
---
id: generator-opts-server-scala-play-server
title: Config Options for scala-play-server
sidebar_label: scala-play-server
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
|routesFileName|Name of the routes file to generate.| |routes|
|routesFileName|Base package in which supporting classes are generated.| |org.openapitools|
|skipStubs|If set, skips generation of stub classes.| |false|
|supportAsync|If set, wraps API return types with Futures and generates async actions.| |false|
|generateCustomExceptions|If set, generates custom exception types.| |true|
|useSwaggerUI|Add a route to /api which show your documentation in swagger-ui. Will also import needed dependencies| |true|
