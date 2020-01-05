---
title: Config Options for swift5
sidebar_label: swift5
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|projectName|Project name in Xcode| |null|
|responseAs|Optionally use libraries to manage response.  Currently PromiseKit, RxSwift, Result, Combine are available.| |null|
|nonPublicApi|Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.(default: false)| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|objcCompatible|Add additional properties and methods for Objective-C compatibility (default: false)| |null|
|podSource|Source information used for Podspec| |null|
|podVersion|Version used for Podspec| |null|
|podAuthors|Authors used for Podspec| |null|
|podSocialMediaURL|Social Media URL used for Podspec| |null|
|podLicense|License used for Podspec| |null|
|podHomepage|Homepage used for Podspec| |null|
|podSummary|Summary used for Podspec| |null|
|podDescription|Description used for Podspec| |null|
|podScreenshots|Screenshots used for Podspec| |null|
|podDocumentationURL|Documentation URL used for Podspec| |null|
|swiftUseApiNamespace|Flag to make all the API classes inner-class of {{projectName}}API| |null|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|lenientTypeCast|Accept and cast values for simple types (string-&gt;bool, string-&gt;int, int-&gt;string)| |false|
|library|Library template (sub-template) to use|<dl><dt>**urlsession**</dt><dd>[DEFAULT] HTTP client: URLSession</dd><dt>**alamofire**</dt><dd>HTTP client: Alamofire</dd><dl>|urlsession|
