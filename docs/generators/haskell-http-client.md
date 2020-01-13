---
title: Config Options for haskell-http-client
sidebar_label: haskell-http-client
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|cabalPackage|Set the cabal package name, which consists of one or more alphanumeric words separated by hyphens| |null|
|cabalVersion|Set the cabal version number, consisting of a sequence of one or more integers separated by dots| |null|
|baseModule|Set the base module namespace| |null|
|requestType|Set the name of the type used to generate requests| |null|
|configType|Set the name of the type used for configuration| |null|
|allowFromJsonNulls|allow JSON Null during model decoding from JSON| |true|
|allowToJsonNulls|allow emitting JSON Null during model encoding to JSON| |false|
|allowNonUniqueOperationIds|allow different API modules to contain the same operationId. Each API must be imported qualified| |false|
|generateLenses|Generate Lens optics for Models| |true|
|generateModelConstructors|Generate smart constructors (only supply required fields) for models| |true|
|generateEnums|Generate specific datatypes for OpenAPI enums| |true|
|generateFormUrlEncodedInstances|Generate FromForm/ToForm instances for models that are used by operations that produce or consume application/x-www-form-urlencoded| |true|
|inlineMimeTypes|Inline (hardcode) the content-type and accept parameters on operations, when there is only 1 option| |true|
|modelDeriving|Additional classes to include in the deriving() clause of Models| |null|
|strictFields|Add strictness annotations to all model fields| |true|
|useKatip|Sets the default value for the UseKatip cabal flag. If true, the katip package provides logging instead of monad-logger| |true|
|dateTimeFormat|format string used to parse/render a datetime| |null|
|dateTimeParseFormat|overrides the format string used to parse a datetime| |null|
|dateFormat|format string used to parse/render a date| |%Y-%m-%d|
|customTestInstanceModule|test module used to provide typeclass instances for types not known by the generator| |null|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Integer</li>
<li>FilePath</li>
<li>Float</li>
<li>Bool</li>
<li>Char</li>
<li>List</li>
<li>Text</li>
<li>String</li>
<li>Double</li>
<li>Int</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>qualified</li>
<li>instance</li>
<li>data</li>
<li>import</li>
<li>infixr</li>
<li>do</li>
<li>type</li>
<li>pure</li>
<li>foreign</li>
<li>newtype</li>
<li>hiding</li>
<li>rec</li>
<li>default</li>
<li>else</li>
<li>of</li>
<li>let</li>
<li>where</li>
<li>class</li>
<li>if</li>
<li>case</li>
<li>proc</li>
<li>in</li>
<li>forall</li>
<li>module</li>
<li>then</li>
<li>infix</li>
<li>accept</li>
<li>contenttype</li>
<li>as</li>
<li>deriving</li>
<li>infixl</li>
<li>mdo</li>
<li>family</li>
<li>return</li>
</ul>
