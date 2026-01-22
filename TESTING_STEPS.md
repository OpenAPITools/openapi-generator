# Testing Steps Required for HTML Generator Fix

This document outlines the steps that need to be completed to finalize the HTML generator array type fix according to the contribution guidelines.

## What Was Fixed

Fixed the `bodyParam.mustache` template in `/modules/openapi-generator/src/main/resources/htmlDocs/` to properly display array types for request body parameters.

**Before:** `User`
**After:** `array[User]`

## Steps to Complete (Due to Build Environment Limitations)

The following steps could not be completed in the automated environment due to network restrictions preventing Maven from downloading dependencies:

### 1. Build the Project

```bash
./mvnw clean install -DskipTests -Dmaven.javadoc.skip=true
```

This generates the `openapi-generator-cli.jar` needed for sample generation.

### 2. Create HTML Generator Config (Already Done)

Created `/bin/configs/html.yaml` with:
```yaml
generatorName: html
outputDir: samples/documentation/html
inputSpec: modules/openapi-generator/src/test/resources/3_0/petstore.yaml
templateDir: modules/openapi-generator/src/main/resources/htmlDocs
additionalProperties:
  hideGenerationTimestamp: "true"
```

### 3. Regenerate HTML Samples

```bash
./bin/generate-samples.sh bin/configs/html.yaml
```

This will update `/samples/documentation/html/index.html` with the fix.

### 4. Verify the Fix

Check that in `samples/documentation/html/index.html`, the `createUsersWithArrayInput` endpoint now shows:
- Body parameter: `User array[User] (required)` instead of just `User (required)`

Or similar array body parameters show the `array[TypeName]` format.

### 5. Commit the Updated Samples

```bash
git add bin/configs/html.yaml
git add samples/documentation/html/
git commit -m "Add html.yaml config and update HTML samples after array type fix"
```

### 6. Optional: Run Tests

```bash
mvn verify -Psamples
```

### 7. Push All Changes

```bash
git push origin claude/fix-html-array-types-aPDDX
```

## Verification

### Concrete Example from Current Sample

In the existing `samples/documentation/html/index.html` at **line 1037**, the `createUsersWithArrayInput` endpoint currently shows:

```html
<div class="param">User <a href="#User">User</a> (required)</div>
```

**Problem**: This endpoint accepts an `array[User]` but only displays "User"

After regeneration with the fix, line 1037 should show:

```html
<div class="param">User array[<a href="#User">User</a>] (required)</div>
```

**Result**: Will correctly display "User array[User] (required)"

### How to Verify

1. After regenerating samples, open `samples/documentation/html/index.html`
2. Navigate to line 1037 (or search for "createUsersWithArrayInput" and look at the Request body section)
3. Confirm the body parameter shows `array[User]` instead of just `User`

## Current Status

✅ Template fix committed
✅ Config file created
❌ Samples not regenerated (requires build)
❌ Tests not run (requires build)

The template fix is correct and ready, but sample regeneration is required to complete the PR per contribution guidelines.
