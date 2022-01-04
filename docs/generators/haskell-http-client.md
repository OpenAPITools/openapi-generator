---
title: Config Options for haskell-http-client
sidebar_label: haskell-http-client
---

These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

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
|disallowAdditionalPropertiesIfNotPresent|If false, the 'additionalProperties' implementation (set to true by default) is compliant with the OAS and JSON schema specifications. If true (default), keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.</dd></dl>|true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|generateEnums|Generate specific datatypes for OpenAPI enums| |true|
|generateFormUrlEncodedInstances|Generate FromForm/ToForm instances for models that are used by operations that produce or consume application/x-www-form-urlencoded| |true|
|generateLenses|Generate Lens optics for Models| |true|
|generateModelConstructors|Generate smart constructors (only supply required fields) for models| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|inlineMimeTypes|Inline (hardcode) the content-type and accept parameters on operations, when there is only 1 option| |true|
|legacyDiscriminatorBehavior|Set to false for generators with better support for discriminators. (Python, Java, Go, PowerShell, C#have this enabled by default).|<dl><dt>**true**</dt><dd>The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.</dd><dt>**false**</dt><dd>The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.</dd></dl>|true|
|modelDeriving|Additional classes to include in the deriving() clause of Models| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|queryExtraUnreserved|Configures additional querystring characters which must not be URI encoded, e.g. '+' or ':'| |null|
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

<ul class="column-ul">
<li>Bool</li>
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

<ul class="column-ul">
<li>accept</li>
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

## FEATURE SET


### Client Modification Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasePath|✓|ToolingExtension
|Authorizations|✗|ToolingExtension
|UserAgent|✓|ToolingExtension
|MockServer|✗|ToolingExtension

### Data Type Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Custom|✗|OAS2,OAS3
|Int32|✓|OAS2,OAS3
|Int64|✓|OAS2,OAS3
|Float|✓|OAS2,OAS3
|Double|✓|OAS2,OAS3
|Decimal|✓|ToolingExtension
|String|✓|OAS2,OAS3
|Byte|✓|OAS2,OAS3
|Binary|✓|OAS2,OAS3
|Boolean|✓|OAS2,OAS3
|Date|✓|OAS2,OAS3
|DateTime|✓|OAS2,OAS3
|Password|✓|OAS2,OAS3
|File|✓|OAS2
|Array|✓|OAS2,OAS3
|Maps|✓|ToolingExtension
|CollectionFormat|✓|OAS2
|CollectionFormatMulti|✓|OAS2
|Enum|✓|OAS2,OAS3
|ArrayOfEnum|✓|ToolingExtension
|ArrayOfModel|✓|ToolingExtension
|ArrayOfCollectionOfPrimitives|✓|ToolingExtension
|ArrayOfCollectionOfModel|✓|ToolingExtension
|ArrayOfCollectionOfEnum|✓|ToolingExtension
|MapOfEnum|✓|ToolingExtension
|MapOfModel|✓|ToolingExtension
|MapOfCollectionOfPrimitives|✓|ToolingExtension
|MapOfCollectionOfModel|✓|ToolingExtension
|MapOfCollectionOfEnum|✓|ToolingExtension

### Documentation Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Readme|✓|ToolingExtension
|Model|✓|ToolingExtension
|Api|✓|ToolingExtension

### Global Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Host|✓|OAS2,OAS3
|BasePath|✓|OAS2,OAS3
|Info|✓|OAS2,OAS3
|Schemes|✗|OAS2,OAS3
|PartialSchemes|✓|OAS2,OAS3
|Consumes|✓|OAS2
|Produces|✓|OAS2
|ExternalDocumentation|✓|OAS2,OAS3
|Examples|✓|OAS2,OAS3
|XMLStructureDefinitions|✗|OAS2,OAS3
|MultiServer|✗|OAS3
|ParameterizedServer|✗|OAS3
|ParameterStyling|✗|OAS3
|Callbacks|✗|OAS3
|LinkObjects|✗|OAS3

### Parameter Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Path|✓|OAS2,OAS3
|Query|✓|OAS2,OAS3
|Header|✓|OAS2,OAS3
|Body|✓|OAS2
|FormUnencoded|✓|OAS2
|FormMultipart|✓|OAS2
|Cookie|✗|OAS3

### Schema Support Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Simple|✓|OAS2,OAS3
|Composite|✓|OAS2,OAS3
|Polymorphism|✗|OAS2,OAS3
|Union|✗|OAS3

### Security Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasicAuth|✓|OAS2,OAS3
|ApiKey|✓|OAS2,OAS3
|OpenIDConnect|✗|OAS3
|BearerToken|✗|OAS3
|OAuth2_Implicit|✓|OAS2,OAS3
|OAuth2_Password|✗|OAS2,OAS3
|OAuth2_ClientCredentials|✗|OAS2,OAS3
|OAuth2_AuthorizationCode|✗|OAS2,OAS3

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✓|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
