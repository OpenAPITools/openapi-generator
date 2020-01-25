---
title: Config Options for haskell
sidebar_label: haskell
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|apiPackage|package for generated api classes| |null|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|modelPackage|package for generated models| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|serveStatic|serve will serve files from the directory 'static'.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|Map|qualified Data.Map as Map|


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
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>as</li>
<li>case</li>
<li>class</li>
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
<li>qualified</li>
<li>rec</li>
<li>then</li>
<li>type</li>
<li>where</li>
</ul>
