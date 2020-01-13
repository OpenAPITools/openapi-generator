---
title: Config Options for bash
sidebar_label: bash
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|curlOptions|Default cURL options| |null|
|processMarkdown|Convert all Markdown Markup into terminal formatting| |false|
|scriptName|The name of the script that will be generated (e.g. petstore-cli)| |null|
|generateBashCompletion|Whether to generate the Bash completion script| |false|
|generateZshCompletion|Whether to generate the Zsh completion script| |false|
|hostEnvironmentVariable|Name of environment variable where host can be defined (e.g. PETSTORE_HOST='http://api.openapitools.org:8080')| |null|
|basicAuthEnvironmentVariable|Name of environment variable where username and password can be defined (e.g. PETSTORE_CREDS='username:password')| |null|
|apiKeyAuthEnvironmentVariable|Name of environment variable where API key can be defined (e.g. PETSTORE_APIKEY='kjhasdGASDa5asdASD')| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|LocalDateTime|org.joda.time.*|
|Set|java.util.*|
|LocalTime|org.joda.time.*|
|HashMap|java.util.HashMap|
|ArrayList|java.util.ArrayList|
|URI|java.net.URI|
|Timestamp|java.sql.Timestamp|
|LocalDate|org.joda.time.*|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|Array|java.util.List|
|List|java.util.*|
|UUID|java.util.UUID|
|File|java.io.File|
|Map|java.util.Map|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>boolean</li>
<li>string</li>
<li>array</li>
<li>binary</li>
<li>integer</li>
<li>float</li>
<li>map</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>fi</li>
<li>select</li>
<li>in</li>
<li>for</li>
<li>do</li>
<li>elif</li>
<li>then</li>
<li>while</li>
<li>done</li>
<li>else</li>
<li>function</li>
<li>until</li>
<li>time</li>
<li>if</li>
<li>case</li>
<li>esac</li>
</ul>
