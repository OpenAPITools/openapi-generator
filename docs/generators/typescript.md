---
title: Config Options for typescript
sidebar_label: typescript
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|supportsES6|Generate code that conforms to ES6.| |false|
|fileContentDataType|Specifies the type to use for the content of a file - i.e. Blob (Browser) / Buffer (node)| |Buffer|
|useRxJS|Enable this to internally use rxjs observables. If disabled, a stub is used instead. This is required for the 'angular' framework.| |false|
|framework|Specify the framework which should be used in the client code.|<dl><dt>**fetch-api**</dt><dd>fetch-api</dd><dt>**jquery**</dt><dd>jquery</dd><dl>|fetch-api|
