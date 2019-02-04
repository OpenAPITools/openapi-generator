
---
id: generator-opts-client-perl
title: Config Options for perl
sidebar_label: perl
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|moduleName|Perl module name (convention: CamelCase or Long::Module).| |OpenAPIClient|
|moduleVersion|Perl module version.| |1.0.0|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
