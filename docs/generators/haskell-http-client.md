
---
id: generator-opts-client-haskell-http-client
title: Config Options for haskell-http-client
sidebar_label: haskell-http-client
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
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
|dateFormat|format string used to parse/render a date| |%Y-%m-%d|
|customTestInstanceModule|test module used to provide typeclass instances for types not known by the generator| |null|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
