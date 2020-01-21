---
title: Config Options for fsharp-giraffe-server
sidebar_label: fsharp-giraffe-server
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|buildTarget|Target the build for a program or library.| |program|
|generateBody|Generates method body.| |true|
|licenseName|The name of the license| |NoLicense|
|licenseUrl|The URL of the license| |http://localhost|
|packageAuthors|Specifies Authors property in the .NET Core project file.| |OpenAPI|
|packageCopyright|Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |No Copyright|
|packageGuid|The GUID that will be associated with the C# project| |null|
|packageName|F# module name (convention: Title.Case).| |OpenAPI|
|packageTitle|Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |OpenAPI Library|
|packageVersion|F# package version.| |1.0.0|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |OpenAPI/src|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useSwashbuckle|Uses the Swashbuckle.AspNetCore NuGet package for documentation.| |false|

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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Collection</li>
<li>DataTimeOffset</li>
<li>DateTime</li>
<li>Dictionary</li>
<li>Double</li>
<li>ICollection</li>
<li>Int32</li>
<li>Int64</li>
<li>List</li>
<li>String</li>
<li>System.IO.Stream</li>
<li>bool</li>
<li>byte[]</li>
<li>char</li>
<li>decimal</li>
<li>dict</li>
<li>double</li>
<li>float</li>
<li>float32</li>
<li>int</li>
<li>int16</li>
<li>int64</li>
<li>list</li>
<li>nativeint</li>
<li>obj</li>
<li>seq</li>
<li>single</li>
<li>string</li>
<li>uint16</li>
<li>uint32</li>
<li>uint64</li>
<li>unativeint</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>abstract</li>
<li>and</li>
<li>as</li>
<li>assert</li>
<li>async</li>
<li>await</li>
<li>base</li>
<li>begin</li>
<li>bool</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>checked</li>
<li>class</li>
<li>const</li>
<li>continue</li>
<li>decimal</li>
<li>default</li>
<li>delegate</li>
<li>do</li>
<li>done</li>
<li>double</li>
<li>downcast</li>
<li>downto</li>
<li>dynamic</li>
<li>elif</li>
<li>else</li>
<li>end</li>
<li>enum</li>
<li>event</li>
<li>exception</li>
<li>explicit</li>
<li>extern</li>
<li>false</li>
<li>finally</li>
<li>fixed</li>
<li>float</li>
<li>for</li>
<li>foreach</li>
<li>fun</li>
<li>function</li>
<li>if</li>
<li>in</li>
<li>inherit</li>
<li>inline</li>
<li>int</li>
<li>interface</li>
<li>internal</li>
<li>is</li>
<li>lazy</li>
<li>let</li>
<li>let!</li>
<li>localVarFileParams</li>
<li>localVarFormParams</li>
<li>localVarHeaderParams</li>
<li>localVarHttpContentType</li>
<li>localVarHttpContentTypes</li>
<li>localVarHttpHeaderAccept</li>
<li>localVarHttpHeaderAccepts</li>
<li>localVarPath</li>
<li>localVarPathParams</li>
<li>localVarPostBody</li>
<li>localVarQueryParams</li>
<li>localVarResponse</li>
<li>localVarStatusCode</li>
<li>lock</li>
<li>match</li>
<li>match!</li>
<li>member</li>
<li>module</li>
<li>mutable</li>
<li>namespace</li>
<li>new</li>
<li>not</li>
<li>null</li>
<li>of</li>
<li>open</li>
<li>option</li>
<li>or</li>
<li>override</li>
<li>params</li>
<li>private</li>
<li>public</li>
<li>raise</li>
<li>rec</li>
<li>return</li>
<li>return!</li>
<li>sealed</li>
<li>select</li>
<li>static</li>
<li>string</li>
<li>struct</li>
<li>then</li>
<li>to</li>
<li>true</li>
<li>try</li>
<li>type</li>
<li>upcast</li>
<li>use</li>
<li>use!</li>
<li>val</li>
<li>void</li>
<li>volatile</li>
<li>when</li>
<li>while</li>
<li>with</li>
<li>yield</li>
<li>yield!</li>
</ul>
