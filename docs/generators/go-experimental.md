---
title: Config Options for go-experimental
sidebar_label: go-experimental
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|Go package name (convention: lowercase).| |openapi|
|packageVersion|Go package version.| |1.0.0|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|isGoSubmodule|whether the generated Go module is a submodule| |false|
|withGoCodegenComment|whether to include Go codegen comment to disable Go Lint and collapse by default GitHub in PRs and diffs| |false|
|withXml|whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML)| |false|
|enumClassPrefix|Prefix enum with class name| |false|
|structPrefix|whether to prefix struct with the class name. e.g. DeletePetOpts =&gt; PetApiDeletePetOpts| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
