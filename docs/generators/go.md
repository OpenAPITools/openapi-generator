---
title: Config Options for go
sidebar_label: go
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
|withAWSV4Signature|whether to include AWS v4 signature support| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>string</li>
<li>bool</li>
<li>byte</li>
<li>float32</li>
<li>float64</li>
<li>uint</li>
<li>int</li>
<li>complex64</li>
<li>rune</li>
<li>int32</li>
<li>int64</li>
<li>complex128</li>
<li>uint64</li>
<li>uint32</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>struct</li>
<li>defer</li>
<li>select</li>
<li>string</li>
<li>bool</li>
<li>const</li>
<li>import</li>
<li>for</li>
<li>range</li>
<li>float64</li>
<li>interface</li>
<li>type</li>
<li>error</li>
<li>complex64</li>
<li>rune</li>
<li>switch</li>
<li>nil</li>
<li>default</li>
<li>goto</li>
<li>int64</li>
<li>else</li>
<li>continue</li>
<li>int8</li>
<li>uint32</li>
<li>uint16</li>
<li>map</li>
<li>if</li>
<li>case</li>
<li>package</li>
<li>break</li>
<li>byte</li>
<li>var</li>
<li>go</li>
<li>float32</li>
<li>uint</li>
<li>int</li>
<li>int16</li>
<li>func</li>
<li>int32</li>
<li>complex128</li>
<li>uint64</li>
<li>uint8</li>
<li>chan</li>
<li>fallthrough</li>
<li>uintptr</li>
<li>return</li>
</ul>
