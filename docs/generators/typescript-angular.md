
---
id: generator-opts-client-typescript-angular
title: Config Options for typescript-angular
sidebar_label: typescript-angular
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|supportsES6|Generate code that conforms to ES6.| |false|
|npmName|The name under which you want to publish generated npm package. Required to generate a full angular package| |null|
|npmVersion|The version of your npm package. Default is '1.0.0'| |null|
|npmRepository|Use this property to set an url your private npmRepo in the package.json| |null|
|snapshot|When setting this property to true the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm| |false|
|withInterfaces|Setting this property to true will generate interfaces next to the default class implementations.| |false|
|taggedUnions|Use discriminators to create tagged unions instead of extending interfaces.| |false|
|providedInRoot|Use this property to provide Injectables in root (it is only valid in angular version greater or equal to 6.0.0).| |false|
|ngVersion|The version of Angular. Default is '7.0.0'| |null|
|serviceSuffix|The suffix of the generated service. Default is 'Service'.| |null|
|serviceFileSuffix|The suffix of the file of the generated service (service&lt;suffix&gt;.ts). Default is '.service'.| |null|
|modelSuffix|The suffix of the generated model. Default is ''.| |null|
|modelFileSuffix|The suffix of the file of the generated model (model&lt;suffix&gt;.ts). Default is ''.| |null|
|fileNaming|Naming convention for the output files: 'camelCase', 'kebab-case'. Default is 'camelCase'.| |null|
