---
title: Config Options for fsharp-giraffe-server
sidebar_label: fsharp-giraffe-server
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|licenseUrl|The URL of the license| |http://localhost|
|licenseName|The name of the license| |NoLicense|
|packageCopyright|Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |No Copyright|
|packageAuthors|Specifies Authors property in the .NET Core project file.| |OpenAPI|
|packageTitle|Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |OpenAPI Library|
|packageName|F# module name (convention: Title.Case).| |OpenAPI|
|packageVersion|F# package version.| |1.0.0|
|packageGuid|The GUID that will be associated with the C# project| |null|
|sourceFolder|source folder for generated code| |OpenAPI/src|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|useSwashbuckle|Uses the Swashbuckle.AspNetCore NuGet package for documentation.| |false|
|generateBody|Generates method body.| |true|
|buildTarget|Target the build for a program or library.| |program|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|IDictionary|System.Collections.Generic|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|list|
|list|list|
|map|IDictionary|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Dictionary</li>
<li>string</li>
<li>bool</li>
<li>String</li>
<li>System.IO.Stream</li>
<li>float</li>
<li>DateTime</li>
<li>int64</li>
<li>Int32</li>
<li>DataTimeOffset</li>
<li>dict</li>
<li>List</li>
<li>unativeint</li>
<li>uint32</li>
<li>uint16</li>
<li>seq</li>
<li>nativeint</li>
<li>double</li>
<li>float32</li>
<li>list</li>
<li>Double</li>
<li>int</li>
<li>int16</li>
<li>byte[]</li>
<li>single</li>
<li>Int64</li>
<li>obj</li>
<li>char</li>
<li>ICollection</li>
<li>Collection</li>
<li>uint64</li>
<li>decimal</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>exception</li>
<li>struct</li>
<li>select</li>
<li>type</li>
<li>when</li>
<li>localVarQueryParams</li>
<li>else</li>
<li>mutable</li>
<li>lock</li>
<li>let</li>
<li>localVarPathParams</li>
<li>catch</li>
<li>if</li>
<li>case</li>
<li>val</li>
<li>localVarHttpHeaderAccepts</li>
<li>localVarPostBody</li>
<li>in</li>
<li>byte</li>
<li>double</li>
<li>module</li>
<li>is</li>
<li>elif</li>
<li>then</li>
<li>params</li>
<li>enum</li>
<li>explicit</li>
<li>as</li>
<li>begin</li>
<li>internal</li>
<li>yield!</li>
<li>lazy</li>
<li>localVarHttpHeaderAccept</li>
<li>use!</li>
<li>delegate</li>
<li>default</li>
<li>localVarHttpContentTypes</li>
<li>localVarHttpContentType</li>
<li>let!</li>
<li>assert</li>
<li>yield</li>
<li>member</li>
<li>override</li>
<li>event</li>
<li>break</li>
<li>downto</li>
<li>abstract</li>
<li>match!</li>
<li>char</li>
<li>localVarFileParams</li>
<li>to</li>
<li>fun</li>
<li>open</li>
<li>return</li>
<li>use</li>
<li>return!</li>
<li>extern</li>
<li>do</li>
<li>float</li>
<li>while</li>
<li>rec</li>
<li>continue</li>
<li>function</li>
<li>raise</li>
<li>checked</li>
<li>dynamic</li>
<li>new</li>
<li>static</li>
<li>void</li>
<li>upcast</li>
<li>localVarResponse</li>
<li>sealed</li>
<li>finally</li>
<li>done</li>
<li>null</li>
<li>localVarPath</li>
<li>true</li>
<li>fixed</li>
<li>try</li>
<li>decimal</li>
<li>option</li>
<li>private</li>
<li>bool</li>
<li>const</li>
<li>string</li>
<li>for</li>
<li>interface</li>
<li>foreach</li>
<li>not</li>
<li>public</li>
<li>localVarStatusCode</li>
<li>and</li>
<li>of</li>
<li>await</li>
<li>end</li>
<li>class</li>
<li>localVarFormParams</li>
<li>or</li>
<li>false</li>
<li>match</li>
<li>volatile</li>
<li>int</li>
<li>async</li>
<li>with</li>
<li>localVarHeaderParams</li>
<li>inline</li>
<li>downcast</li>
<li>inherit</li>
<li>namespace</li>
<li>base</li>
</ul>
