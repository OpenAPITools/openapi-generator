---
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

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|std::vector|#include &lt;vector&gt;|
|utility::string_t|#include &lt;cpprest/details/basic_types.h&gt;|
|std::map|#include &lt;map&gt;|
|std::string|#include &lt;string&gt;|
|utility::datetime|#include &lt;cpprest/details/basic_types.h&gt;|
|Object|#include &quot;Object.h&quot;|
|HttpContent|#include &quot;HttpContent.h&quot;|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>bool</li>
<li>double</li>
<li>char</li>
<li>float</li>
<li>int64_t</li>
<li>int</li>
<li>long</li>
<li>int32_t</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>struct</li>
<li>auto</li>
<li>xor_eq</li>
<li>const_cast</li>
<li>decltype</li>
<li>alignas</li>
<li>extern</li>
<li>do</li>
<li>float</li>
<li>while</li>
<li>constexpr</li>
<li>operator</li>
<li>bitand</li>
<li>protected</li>
<li>continue</li>
<li>else</li>
<li>friend</li>
<li>mutable</li>
<li>compl</li>
<li>typeid</li>
<li>catch</li>
<li>export</li>
<li>if</li>
<li>case</li>
<li>dynamic_cast</li>
<li>not_eq</li>
<li>new</li>
<li>using</li>
<li>static</li>
<li>void</li>
<li>sizeof</li>
<li>bitor</li>
<li>double</li>
<li>this</li>
<li>signed</li>
<li>noexcept</li>
<li>typedef</li>
<li>enum</li>
<li>char16_t</li>
<li>explicit</li>
<li>static_cast</li>
<li>true</li>
<li>try</li>
<li>reinterpret_cast</li>
<li>nullptr</li>
<li>requires</li>
<li>template</li>
<li>private</li>
<li>virtual</li>
<li>bool</li>
<li>const</li>
<li>concept</li>
<li>static_assert</li>
<li>for</li>
<li>delete</li>
<li>long</li>
<li>switch</li>
<li>default</li>
<li>not</li>
<li>goto</li>
<li>public</li>
<li>and</li>
<li>and_eq</li>
<li>linux</li>
<li>or_eq</li>
<li>xor</li>
<li>class</li>
<li>wchar_t</li>
<li>alignof</li>
<li>or</li>
<li>break</li>
<li>false</li>
<li>thread_local</li>
<li>char32_t</li>
<li>volatile</li>
<li>union</li>
<li>int</li>
<li>inline</li>
<li>throw</li>
<li>char</li>
<li>namespace</li>
<li>short</li>
<li>unsigned</li>
<li>asm</li>
<li>return</li>
<li>typename</li>
<li>register</li>
</ul>
