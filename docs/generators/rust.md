
---
id: generator-opts-client-rust
title: Config Options for rust
sidebar_label: rust
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|Rust package name (convention: lowercase).| |openapi|
|packageVersion|Rust package version.| |1.0.0|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|library|library template (sub-template) to use.|<dl><dt>**hyper**</dt><dd>HTTP client: Hyper.</dd><dt>**reqwest**</dt><dd>HTTP client: Reqwest.</dd><dl>|hyper|
