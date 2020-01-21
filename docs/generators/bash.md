---
title: Config Options for bash
sidebar_label: bash
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|apiKeyAuthEnvironmentVariable|Name of environment variable where API key can be defined (e.g. PETSTORE_APIKEY='kjhasdGASDa5asdASD')| |false|
|basicAuthEnvironmentVariable|Name of environment variable where username and password can be defined (e.g. PETSTORE_CREDS='username:password')| |null|
|curlOptions|Default cURL options| |null|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|generateBashCompletion|Whether to generate the Bash completion script| |false|
|generateZshCompletion|Whether to generate the Zsh completion script| |false|
|hostEnvironmentVariable|Name of environment variable where host can be defined (e.g. PETSTORE_HOST='http://api.openapitools.org:8080')| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|processMarkdown|Convert all Markdown Markup into terminal formatting| |false|
|scriptName|The name of the script that will be generated (e.g. petstore-cli)| |null|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|Array|java.util.List|
|ArrayList|java.util.ArrayList|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|File|java.io.File|
|HashMap|java.util.HashMap|
|List|java.util.*|
|LocalDate|org.joda.time.*|
|LocalDateTime|org.joda.time.*|
|LocalTime|org.joda.time.*|
|Map|java.util.Map|
|Set|java.util.*|
|Timestamp|java.sql.Timestamp|
|URI|java.net.URI|
|UUID|java.util.UUID|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>array</li>
<li>binary</li>
<li>boolean</li>
<li>float</li>
<li>integer</li>
<li>map</li>
<li>string</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>case</li>
<li>do</li>
<li>done</li>
<li>elif</li>
<li>else</li>
<li>esac</li>
<li>fi</li>
<li>for</li>
<li>function</li>
<li>if</li>
<li>in</li>
<li>select</li>
<li>then</li>
<li>time</li>
<li>until</li>
<li>while</li>
</ul>
