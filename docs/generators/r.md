
---
id: generator-opts-client-r
title: Config Options for r
sidebar_label: r
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|R package name (convention: lowercase).| |openapi|
|packageVersion|R package version.| |1.0.0|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|returnExceptionOnFailure|Throw an exception on non success response codes| |false|
|exceptionPackage|Specify the exception handling package|<dl><dt>**default**</dt><dd>Use stop() for raising exceptions.</dd><dt>**rlang**</dt><dd>Use rlang package for exceptions.</dd><dl>|default|
