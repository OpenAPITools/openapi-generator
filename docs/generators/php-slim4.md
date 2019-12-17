---
title: Config Options for php-slim4
sidebar_label: php-slim4
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPackage|package for generated models| |null|
|apiPackage|package for generated api classes| |null|
|variableNamingConvention|naming convention of variable name, e.g. camelCase.| |camelCase|
|invokerPackage|The main namespace to use for all classes. e.g. Yay\Pets| |null|
|packageName|The main package name for classes. e.g. GeneratedPetstore| |null|
|srcBasePath|The directory to serve as source root.| |null|
|artifactVersion|The version to use in the composer package version field. e.g. 1.2.3| |null|
|psr7Implementation|Slim 4 provides its own PSR-7 implementation so that it works out of the box. However, you are free to replace Slim&rsquo;s default PSR-7 objects with a third-party implementation. Ref: https://www.slimframework.com/docs/v4/concepts/value-objects.html|<dl><dt>**slim-psr7**</dt><dd>Slim PSR-7 Message implementation</dd><dt>**nyholm-psr7**</dt><dd>Nyholm PSR-7 Message implementation</dd><dt>**guzzle-psr7**</dt><dd>Guzzle PSR-7 Message implementation</dd><dt>**zend-diactoros**</dt><dd>Zend Diactoros PSR-7 Message implementation</dd><dl>|slim-psr7|
