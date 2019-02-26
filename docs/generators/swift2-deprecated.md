
---
id: generator-opts-client-swift2-deprecated
title: Config Options for swift2-deprecated
sidebar_label: swift2-deprecated
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|projectName|Project name in Xcode| |null|
|responseAs|Optionally use libraries to manage response.  Currently PromiseKit, RxSwift are available.| |null|
|unwrapRequired|Treat 'required' properties in response as non-optional (which would crash the app if api returns null as opposed to required option specified in json schema| |null|
|podSource|Source information used for Podspec| |null|
|podVersion|Version used for Podspec| |null|
|podAuthors|Authors used for Podspec| |null|
|podSocialMediaURL|Social Media URL used for Podspec| |null|
|podDocsetURL|Docset URL used for Podspec| |null|
|podLicense|License used for Podspec| |null|
|podHomepage|Homepage used for Podspec| |null|
|podSummary|Summary used for Podspec| |null|
|podDescription|Description used for Podspec| |null|
|podScreenshots|Screenshots used for Podspec| |null|
|podDocumentationURL|Documentation URL used for Podspec| |null|
|swiftUseApiNamespace|Flag to make all the API classes inner-class of {{projectName}}API| |null|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
