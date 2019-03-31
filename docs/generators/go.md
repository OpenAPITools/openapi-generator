
---
id: generator-opts-client-go
title: Config Options for go
sidebar_label: go
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|Go package name (convention: lowercase).| |openapi|
|packageVersion|Go package version.| |1.0.0|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|withGoCodegenComment|whether to include Go codegen comment to disable Go Lint and collapse by default GitHub in PRs and diffs| |false|
|withXml|whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML)| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
