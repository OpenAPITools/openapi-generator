
---
id: generator-opts-client-cpp-restsdk
title: Config Options for cpp-restsdk
sidebar_label: cpp-restsdk
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|modelPackage|C++ namespace for models (convention: name.space.model).| |org.openapitools.client.model|
|apiPackage|C++ namespace for apis (convention: name.space.api).| |org.openapitools.client.api|
|packageVersion|C++ package version.| |1.0.0|
|declspec|C++ preprocessor to place before the class name for handling dllexport/dllimport.| ||
|defaultInclude|The default include statement that should be placed in all headers for including things like the declspec (convention: #include &quot;Commons.h&quot; | ||
|generateGMocksForApis|Generate Google Mock classes for APIs.| |null|
