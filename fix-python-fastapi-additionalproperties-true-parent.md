# Fix Plan: python-fastapi additionalProperties:true generates wrong parent class

## Issue
GitHub issue #20153: `python-fastapi` generator produces invalid imports and wrong base class for
models that have `additionalProperties: true` (a boolean) at the schema level.

**Broken output:**
```python
from openapi_server.models.object import object
class HTTPRequest(object):
    ...
```

**Expected output:**
```python
from pydantic import BaseModel
class HTTPRequest(BaseModel):
    ...
```

## Root Cause

`ModelUtils.isMapSchema()` returns `true` for any schema with `additionalProperties: true` because
the boolean value counts as "map-like" per the OAS spec. This triggers
`updateModelForObject()` → `addAdditionPropertiesToCodeGenModel()` → `addParentContainer()` →
`addParentFromContainer()`, which sets `model.parent = toInstantiationType(schema)`.

For `additionalProperties: true`, `ModelUtils.getAdditionalProperties()` returns `new Schema()`
(an empty schema), and `getSchemaType(new Schema())` returns `"object"`.

So `model.parent` becomes `"object"` — Python's built-in type. Then in
`AbstractPythonCodegen.postProcessModels()`:

```java
if (!StringUtils.isEmpty(model.parent)) {
    modelImports.add(model.parent);  // → "from openapi_server.models.object import object"
}
```

And the mustache template:
```mustache
class {{classname}}({{#parent}}{{{.}}}{{/parent}}{{^parent}}BaseModel{{/parent}}):
```

generates `class HTTPRequest(object):` instead of `class HTTPRequest(BaseModel):`.

`PythonClientCodegen` already overrides `addAdditionPropertiesToCodeGenModel()` to avoid this,
but `PythonFastAPIServerCodegen` and `AbstractPythonConnexionServerCodegen` do not — they fall
through to `DefaultCodegen` which calls `addParentContainer()`.

## Fix

Move the correct `addAdditionPropertiesToCodeGenModel()` override from `PythonClientCodegen` up
to `AbstractPythonCodegen`. This ensures ALL Python generators (FastAPI, Connexion, client) share
the same correct behaviour without duplicating the fix.

```java
// AbstractPythonCodegen.java
@Override
protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
    final Schema additionalProperties = ModelUtils.getAdditionalProperties(schema);
    if (additionalProperties != null) {
        codegenModel.additionalPropertiesType = getSchemaType(additionalProperties);
    }
    // Do NOT call addParentContainer() — Python uses the `additional_properties` field
    // pattern in templates rather than class inheritance to represent additionalProperties.
    // Calling the default would set model.parent = "object" which generates an invalid
    // "from openapi_server.models.object import object" import.
}
```

Remove the duplicate override from `PythonClientCodegen`.

## Test

Add a regression test in `PythonFastAPIServerCodegenTest` using a new fixture spec
`src/test/resources/bugs/issue_20153.yaml` that includes a schema with
`additionalProperties: true` and named properties (modelling the TAMS `http-request.json`).

The test verifies:
- `from openapi_server.models.object import object` is NOT present in the generated model
- `class HttpRequest(BaseModel):` IS present (correct base class)
- `additional_properties: Dict[str, Any] = {}` IS present (the correct pattern)

## Files Changed

1. `modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/AbstractPythonCodegen.java`
   — add `addAdditionPropertiesToCodeGenModel()` override

2. `modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/PythonClientCodegen.java`
   — remove now-duplicate `addAdditionPropertiesToCodeGenModel()` override

3. `modules/openapi-generator/src/test/resources/bugs/issue_20153.yaml`
   — new minimal OAS 3.1 fixture

4. `modules/openapi-generator/src/test/java/org/openapitools/codegen/python/PythonFastAPIServerCodegenTest.java`
   — new regression test `testAdditionalPropertiesTrueUsesBaseModel`
