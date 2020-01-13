---
title: Config Options for haskell
sidebar_label: haskell
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPackage|package for generated models| |null|
|apiPackage|package for generated api classes| |null|
|serveStatic|serve will serve files from the directory 'static'.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|Map|qualified Data.Map as Map|


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
<li>as</li>
<li>deriving</li>
<li>infixl</li>
<li>mdo</li>
<li>family</li>
</ul>
