---
title: Config Options for cpp-restsdk
sidebar_label: cpp-restsdk
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|apiPackage|C++ namespace for apis (convention: name.space.api).| |org.openapitools.client.api|
|declspec|C++ preprocessor to place before the class name for handling dllexport/dllimport.| ||
|defaultInclude|The default include statement that should be placed in all headers for including things like the declspec (convention: #include &quot;Commons.h&quot; | ||
|generateGMocksForApis|Generate Google Mock classes for APIs.| |null|
|modelPackage|C++ namespace for models (convention: name.space.model).| |org.openapitools.client.model|
|packageVersion|C++ package version.| |1.0.0|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|HttpContent|#include &quot;HttpContent.h&quot;|
|Object|#include &quot;Object.h&quot;|
|std::map|#include &lt;map&gt;|
|std::string|#include &lt;string&gt;|
|std::vector|#include &lt;vector&gt;|
|utility::datetime|#include &lt;cpprest/details/basic_types.h&gt;|
|utility::string_t|#include &lt;cpprest/details/basic_types.h&gt;|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>bool</li>
<li>char</li>
<li>double</li>
<li>float</li>
<li>int</li>
<li>int32_t</li>
<li>int64_t</li>
<li>long</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>alignas</li>
<li>alignof</li>
<li>and</li>
<li>and_eq</li>
<li>asm</li>
<li>auto</li>
<li>bitand</li>
<li>bitor</li>
<li>bool</li>
<li>break</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>char16_t</li>
<li>char32_t</li>
<li>class</li>
<li>compl</li>
<li>concept</li>
<li>const</li>
<li>const_cast</li>
<li>constexpr</li>
<li>continue</li>
<li>decltype</li>
<li>default</li>
<li>delete</li>
<li>do</li>
<li>double</li>
<li>dynamic_cast</li>
<li>else</li>
<li>enum</li>
<li>explicit</li>
<li>export</li>
<li>extern</li>
<li>false</li>
<li>float</li>
<li>for</li>
<li>friend</li>
<li>goto</li>
<li>if</li>
<li>inline</li>
<li>int</li>
<li>linux</li>
<li>long</li>
<li>mutable</li>
<li>namespace</li>
<li>new</li>
<li>noexcept</li>
<li>not</li>
<li>not_eq</li>
<li>nullptr</li>
<li>operator</li>
<li>or</li>
<li>or_eq</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>register</li>
<li>reinterpret_cast</li>
<li>requires</li>
<li>return</li>
<li>short</li>
<li>signed</li>
<li>sizeof</li>
<li>static</li>
<li>static_assert</li>
<li>static_cast</li>
<li>struct</li>
<li>switch</li>
<li>template</li>
<li>this</li>
<li>thread_local</li>
<li>throw</li>
<li>true</li>
<li>try</li>
<li>typedef</li>
<li>typeid</li>
<li>typename</li>
<li>union</li>
<li>unsigned</li>
<li>using</li>
<li>virtual</li>
<li>void</li>
<li>volatile</li>
<li>wchar_t</li>
<li>while</li>
<li>xor</li>
<li>xor_eq</li>
</ul>
