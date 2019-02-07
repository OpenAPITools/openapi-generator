
---
id: generator-opts-client-objc
title: Config Options for objc
sidebar_label: objc
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|coreData|Should generate core data models| |false|
|classPrefix|prefix for generated classes (convention: Abbreviation of pod name e.g. `HN` for `HackerNews`).`| |OAI|
|podName|cocoapods package name (convention: CameCase).| |OpenAPIClient|
|podVersion|cocoapods package version.| |1.0.0|
|authorName|Name to use in the podspec file.| |OpenAPI|
|authorEmail|Email to use in the podspec file.| |team@openapitools.org|
|gitRepoURL|URL for the git repo where this podspec should point to.| |https://github.com/openapitools/openapi-generator|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
