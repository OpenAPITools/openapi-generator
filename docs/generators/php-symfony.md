
---
id: generator-opts-server-php-symfony
title: Config Options for php-symfony
sidebar_label: php-symfony
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPackage|package for generated models| |null|
|apiPackage|package for generated api classes| |null|
|variableNamingConvention|naming convention of variable name, e.g. camelCase.| |snake_case|
|invokerPackage|The main namespace to use for all classes. e.g. Yay\Pets| |null|
|packageName|The main package name for classes. e.g. GeneratedPetstore| |null|
|srcBasePath|The directory to serve as source root.| |null|
|artifactVersion|The version to use in the composer package version field. e.g. 1.2.3| |null|
|composerVendorName|The vendor name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. yaypets| |null|
|bundleName|The name of the Symfony bundle. The template uses {{bundleName}}| |null|
|composerProjectName|The project name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. petstore-client| |null|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|phpLegacySupport|Should the generated code be compatible with PHP 5.x?| |true|
