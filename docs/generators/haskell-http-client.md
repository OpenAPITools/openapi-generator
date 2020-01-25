---
title: Config Options for haskell-http-client
sidebar_label: haskell-http-client
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowFromJsonNulls|allow JSON Null during model decoding from JSON| |true|
|allowNonUniqueOperationIds|allow different API modules to contain the same operationId. Each API must be imported qualified| |false|
|allowToJsonNulls|allow emitting JSON Null during model encoding to JSON| |false|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|baseModule|Set the base module namespace| |null|
|cabalPackage|Set the cabal package name, which consists of one or more alphanumeric words separated by hyphens| |null|
|cabalVersion|Set the cabal version number, consisting of a sequence of one or more integers separated by dots| |null|
|configType|Set the name of the type used for configuration| |null|
|customTestInstanceModule|test module used to provide typeclass instances for types not known by the generator| |null|
|dateFormat|format string used to parse/render a date| |%Y-%m-%d|
|dateTimeFormat|format string used to parse/render a datetime| |null|
|dateTimeParseFormat|overrides the format string used to parse a datetime| |null|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|generateEnums|Generate specific datatypes for OpenAPI enums| |true|
|generateFormUrlEncodedInstances|Generate FromForm/ToForm instances for models that are used by operations that produce or consume application/x-www-form-urlencoded| |true|
|generateLenses|Generate Lens optics for Models| |true|
|generateModelConstructors|Generate smart constructors (only supply required fields) for models| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|inlineMimeTypes|Inline (hardcode) the content-type and accept parameters on operations, when there is only 1 option| |true|
|modelDeriving|Additional classes to include in the deriving() clause of Models| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|requestType|Set the name of the type used to generate requests| |null|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|strictFields|Add strictness annotations to all model fields| |true|
|useKatip|Sets the default value for the UseKatip cabal flag. If true, the katip package provides logging instead of monad-logger| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Bool</li>
<li>Char</li>
<li>Double</li>
<li>FilePath</li>
<li>Float</li>
<li>Int</li>
<li>Integer</li>
<li>List</li>
<li>String</li>
<li>Text</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>accept</li>
<li>as</li>
<li>case</li>
<li>class</li>
<li>contenttype</li>
<li>data</li>
<li>default</li>
<li>deriving</li>
<li>do</li>
<li>else</li>
<li>family</li>
<li>forall</li>
<li>foreign</li>
<li>hiding</li>
<li>if</li>
<li>import</li>
<li>in</li>
<li>infix</li>
<li>infixl</li>
<li>infixr</li>
<li>instance</li>
<li>let</li>
<li>mdo</li>
<li>module</li>
<li>newtype</li>
<li>of</li>
<li>proc</li>
<li>pure</li>
<li>qualified</li>
<li>rec</li>
<li>return</li>
<li>then</li>
<li>type</li>
<li>where</li>
</ul>
