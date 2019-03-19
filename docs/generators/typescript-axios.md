
---
id: generator-opts-client-typescript-axios
title: Config Options for typescript-axios
sidebar_label: typescript-axios
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|supportsES6|Generate code that conforms to ES6.| |false|
|npmName|The name under which you want to publish generated npm package| |null|
|npmVersion|The version of your npm package| |null|
|npmRepository|Use this property to set an url your private npmRepo in the package.json| |null|
|snapshot|When setting this property to true the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm| |false|
|withInterfaces|Setting this property to true will generate interfaces next to the default class implementations.| |false|
|withSeparateModelsAndApi|Put the model and api in separate folders and in separate classes| |false|
