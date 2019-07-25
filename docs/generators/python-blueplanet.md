
---
id: generator-opts-server-python-blueplanet
title: Config Options for python-blueplanet
sidebar_label: python-blueplanet
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|packageName|python package name (convention: snake_case).| |openapi_server|
|packageVersion|python package version.| |1.0.0|
|controllerPackage|controller package| |controllers|
|defaultController|default controller| |default_controller|
|supportPython2|support python2| |false|
|serverPort|TCP port to listen to in app.run| |8080|
