
---
id: generator-opts-client-clojure
title: Config Options for clojure
sidebar_label: clojure
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|projectName|name of the project (Default: generated from info.title or &quot;openapi-clj-client&quot;)| |null|
|projectDescription|description of the project (Default: using info.description or &quot;Client library of &lt;projectName&gt;&quot;)| |null|
|projectVersion|version of the project (Default: using info.version or &quot;1.0.0&quot;)| |null|
|projectUrl|URL of the project (Default: using info.contact.url or not included in project.clj)| |null|
|projectLicenseName|name of the license the project uses (Default: using info.license.name or not included in project.clj)| |null|
|projectLicenseUrl|URL of the license the project uses (Default: using info.license.url or not included in project.clj)| |null|
|baseNamespace|the base/top namespace (Default: generated from projectName)| |null|
