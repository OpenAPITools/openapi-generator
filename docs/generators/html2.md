
---
id: generator-opts-documentation-html2
title: Config Options for html2
sidebar_label: html2
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|appName|short name of the application| |null|
|appDescription|description of the application| |null|
|infoUrl|a URL where users can get more information about the application| |null|
|infoEmail|an email address to contact for inquiries about the application| |null|
|licenseInfo|a short description of the license| |null|
|licenseUrl|a URL pointing to the full license| |null|
|invokerPackage|root package for generated code| |null|
|phpInvokerPackage|root package for generated php code| |null|
|perlModuleName|root module name for generated perl code| |null|
|pythonPackageName|package name for generated python code| |null|
|packageName|C# package name| |null|
|groupId|groupId in generated pom.xml| |null|
|artifactId|artifactId in generated pom.xml| |null|
|artifactVersion|artifact version in generated pom.xml| |null|
