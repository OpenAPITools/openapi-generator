
---
id: generator-opts-client-ruby
title: Config Options for ruby
sidebar_label: ruby
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|gemName|gem name (convention: underscore_case).| |openapi_client|
|moduleName|top module name (convention: CamelCase, usually corresponding to gem name).| |OpenAPIClient|
|gemVersion|gem version.| |1.0.0|
|gemLicense|gem license. | |proprietary|
|gemRequiredRubyVersion|gem required Ruby version. | |&gt;= 1.9|
|gemHomepage|gem homepage. | |http://org.openapitools|
|gemSummary|gem summary. | |A ruby wrapper for the REST APIs|
|gemDescription|gem description. | |This gem maps to a REST API|
|gemAuthor|gem author (only one is supported).| |null|
|gemAuthorEmail|gem author email (only one is supported).| |null|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
