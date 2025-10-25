# Plan: Add Backticks to All Julia Template Documentation

## Summary
Add backticks to all identifiers (parameter names, type names, function names, property names) in Julia template documentation to prevent markdown from misinterpreting underscores as italic formatting.

## Files Already Fixed ✅
1. **julia-client/api.mustache** - Docstrings in generated Julia code (DONE)
2. **julia-client/partial_model_doc_oneof.mustache** - Already has backticks
3. **julia-client/partial_model_doc_anyof.mustache** - Already has backticks
4. **julia-server/partial_model_doc_oneof.mustache** - Already has backticks
5. **julia-server/partial_model_doc_anyof.mustache** - Already has backticks

## Files Needing Changes

### Priority 1: Markdown API Documentation

#### 1. julia-client/api_doc.mustache
**Issue**: Function signatures and parameter tables don't use backticks

**Changes needed**:
- Line 14: Function signature - wrap `operationId`, `paramName`, `dataType`, `returnType` in backticks
- Line 15: Streaming signature - same as above
- Line 25: Parameter name `{{paramName}}` → `` `{{paramName}}` ``
- Line 26: Data types - add backticks around type names
- Line 32: Optional parameter name - add backticks
- Line 35-36: Return type - add backticks

#### 2. julia-server/api_doc.mustache
**Issue**: Same as client version

**Changes needed**:
- Line 14: Function signature - wrap `operationId`, `paramName`, `dataType`, `returnType` in backticks
- Line 25: Parameter name - add backticks
- Line 25: Data types - add backticks
- Line 31: Optional parameter name - add backticks
- Line 35: Return type - add backticks

### Priority 2: Markdown Model Documentation

#### 3. julia-client/model_doc.mustache
**Issue**: Property names and types in tables don't use backticks

**Changes needed**:
- Line 13: `**{{name}}**` → `` **`{{name}}`** `` (property names)
- Line 13: Type names - wrap `{{dataType}}` in backticks

#### 4. julia-server/model_doc.mustache
**Issue**: Same as client version

**Changes needed**:
- Same as julia-client/model_doc.mustache

### Priority 3: README Documentation (Optional)

#### 5. julia-client/README.mustache
**Issue**: Identifiers in tables could use backticks for consistency

**Changes needed** (optional):
- Line 32: `` *{{classname}}* `` → `` *`{{classname}}`* ``
- Line 32: `` **{{operationId}}** `` → `` **`{{operationId}}`** ``
- Line 37: `` {{{classname}}} `` → `` `{{{classname}}}` ``

#### 6. julia-server/README.mustache
**Issue**: Same as client version (if exists)

## Implementation Strategy

### Phase 1: API Documentation (High Impact)
1. Fix julia-client/api_doc.mustache
2. Fix julia-server/api_doc.mustache
3. Test with sample regeneration

### Phase 2: Model Documentation (High Impact)
1. Fix julia-client/model_doc.mustache
2. Fix julia-server/model_doc.mustache
3. Test with sample regeneration

### Phase 3: README (Low Impact)
1. Fix README.mustache files (optional enhancement)
2. Final test with sample regeneration

## Testing Checklist

After each change:
- [ ] Build openapi-generator
- [ ] Regenerate Julia client samples: `./bin/generate-samples.sh ./bin/configs/julia-client*`
- [ ] Regenerate Julia server samples: `./bin/generate-samples.sh ./bin/configs/julia-server*`
- [ ] Verify backticks appear correctly in generated markdown files
- [ ] Commit changes with descriptive message

## Expected Outcome

All generated markdown documentation will have identifiers wrapped in backticks:
- `` `pet_id`::`Int64` `` instead of `pet_id::Int64`
- `` `Custom_Type` `` instead of `Custom_Type`
- `` `update_pet_with_form` `` instead of `update_pet_with_form`

This prevents markdown processors from treating underscores as italic markers.
