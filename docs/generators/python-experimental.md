
---
id: generator-opts-client-python-experimental
title: Config Options for python-experimental
sidebar_label: python-experimental
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|python package name (convention: snake_case).| |openapi_client|
|projectName|python project name in setup.py (e.g. petstore-api).| |null|
|packageVersion|python package version.| |1.0.0|
|packageUrl|python package URL.| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|generateSourceCodeOnly|Specifies that only a library source code is to be generated.| |false|
|library|library template (sub-template) to use: asyncio, tornado, urllib3| |urllib3|
