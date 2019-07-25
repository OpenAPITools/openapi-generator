
---
id: generator-opts-client-javascript
title: Config Options for javascript
sidebar_label: javascript
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|sourceFolder|source folder for generated code| |src|
|invokerPackage|root package for generated code| |null|
|apiPackage|package for generated api classes| |null|
|modelPackage|package for generated models| |null|
|projectName|name of the project (Default: generated from info.title or &quot;openapi-js-client&quot;)| |null|
|moduleName|module name for AMD, Node or globals (Default: generated from &lt;projectName&gt;)| |null|
|projectDescription|description of the project (Default: using info.description or &quot;Client library of &lt;projectName&gt;&quot;)| |null|
|projectVersion|version of the project (Default: using info.version or &quot;1.0.0&quot;)| |null|
|licenseName|name of the license the project uses (Default: using info.license.name)| |null|
|usePromises|use Promises as return values from the client API, instead of superagent callbacks| |false|
|emitModelMethods|generate getters and setters for model properties| |false|
|emitJSDoc|generate JSDoc comments| |true|
|useInheritance|use JavaScript prototype chains &amp; delegation for inheritance| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|useES6|use JavaScript ES6 (ECMAScript 6) (beta). Default is ES6.| |true|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
