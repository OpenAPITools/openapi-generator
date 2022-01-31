package org.openapitools.codegen;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.utils.ModelUtils;

public interface IJsonSchemaValidationProperties {
    String getPattern();

    void setPattern(String pattern);

    String getMaximum();

    void setMaximum(String maximum);

    String getMinimum();

    void setMinimum(String minimum);

    boolean getExclusiveMaximum();

    void setExclusiveMaximum(boolean exclusiveMaximum);

    boolean getExclusiveMinimum();

    void setExclusiveMinimum(boolean exclusiveMinimum);

    Integer getMinLength();

    void setMinLength(Integer minLength);

    Integer getMaxLength();

    void setMaxLength(Integer maxLength);

    Integer getMinItems();

    void setMinItems(Integer minItems);

    Integer getMaxItems();

    void setMaxItems(Integer maxItems);

    boolean getUniqueItems();

    void setUniqueItems(boolean uniqueItems);

    Integer getMinProperties();

    void setMinProperties(Integer minProperties);

    Integer getMaxProperties();

    void setMaxProperties(Integer maxProperties);

    Number getMultipleOf();

    void setMultipleOf(Number multipleOf);

    CodegenProperty getItems();

    void setItems(CodegenProperty items);

    boolean getIsModel();

    void setIsModel(boolean isModel);

    boolean getIsDate();

    void setIsDate(boolean isDate);

    boolean getIsDateTime();

    void setIsDateTime(boolean isDateTime);

    // true when the schema type is object
    boolean getIsMap();

    void setIsMap(boolean isMap);

    boolean getIsArray();

    void setIsArray(boolean isShort);

    boolean getIsShort();

    void setIsShort(boolean isShort);

    boolean getIsBoolean();

    void setIsBoolean(boolean isBoolean);

    boolean getIsUnboundedInteger();

    void setIsUnboundedInteger(boolean isUnboundedInteger);

    boolean getIsPrimitiveType();

    void setIsPrimitiveType(boolean isPrimitiveType);

    CodegenProperty getAdditionalProperties();

    void setAdditionalProperties(CodegenProperty additionalProperties);

    List<CodegenProperty> getVars();

    void setVars(List<CodegenProperty> vars);

    List<CodegenProperty> getRequiredVars();

    void setRequiredVars(List<CodegenProperty> requiredVars);

    boolean getIsNull();

    void setIsNull(boolean isNull);

    boolean getHasValidation();

    void setHasValidation(boolean hasValidation);

    boolean getAdditionalPropertiesIsAnyType();

    void setAdditionalPropertiesIsAnyType(boolean additionalPropertiesIsAnyType);

    boolean getHasVars();

    void setHasVars(boolean hasRequiredVars);

    boolean getHasRequired();

    void setHasRequired(boolean hasRequired);

    // discriminators are only supported in request bodies and response payloads per OpenApi
    boolean getHasDiscriminatorWithNonEmptyMapping();

    // discriminators are only supported in request bodies and response payloads per OpenApi
    void setHasDiscriminatorWithNonEmptyMapping(boolean hasDiscriminatorWithNonEmptyMapping);

    boolean getIsString();

    void setIsString(boolean isNumber);

    boolean getIsNumber();

    void setIsNumber(boolean isNumber);

    boolean getIsAnyType();

    void setIsAnyType(boolean isAnyType);

    CodegenComposedSchemas getComposedSchemas();

    void setComposedSchemas(CodegenComposedSchemas composedSchemas);

    boolean getHasMultipleTypes();

    void setHasMultipleTypes(boolean hasMultipleTypes);

    /**
     * Syncs all the schema's type properties into the IJsonSchemaValidationProperties instance
     * for now this only supports types without format information
     * TODO: in the future move the format handling in here too
     * @param p the schema which contains the type info
     */
    default void setTypeProperties(Schema p) {
        if (ModelUtils.isTypeObjectSchema(p)) {
            setIsMap(true);
        } else if (ModelUtils.isArraySchema(p)) {
            setIsArray(true);
        } else if (ModelUtils.isFileSchema(p) && !ModelUtils.isStringSchema(p)) {
            // swagger v2 only, type file
            ;
        } else if (ModelUtils.isStringSchema(p)) {
            setIsString(true);
            if (ModelUtils.isByteArraySchema(p)) {
                ;
            } else if (ModelUtils.isBinarySchema(p)) {
                // openapi v3 way of representing binary + file data
                // for backward compatibility with 2.x file type
                setIsString(false);
            } else if (ModelUtils.isUUIDSchema(p)) {
                // keep isString to true to make it backward compatible
                ;
            } else if (ModelUtils.isURISchema(p)) {
                ;
            } else if (ModelUtils.isEmailSchema(p)) {
                ;
            } else if (ModelUtils.isDateSchema(p)) {
                ;
            } else if (ModelUtils.isDateTimeSchema(p)) {
                ;
            } else if (ModelUtils.isDecimalSchema(p)) { // type: string, format: number
                ;
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (ModelUtils.isFloatSchema(p)) { // float
                ;
            } else if (ModelUtils.isDoubleSchema(p)) { // double
                ;
            } else { // type is number and without format
                setIsNumber(true);
            }
        } else if (ModelUtils.isIntegerSchema(p)) { // integer type
            if (ModelUtils.isLongSchema(p)) { // int64/long format
                ;
            } else if (ModelUtils.isShortSchema(p)) { // int32/short format
                ;
            } else { // unbounded integer
                setIsUnboundedInteger(true);
            }
        } else if (ModelUtils.isBooleanSchema(p)) { // boolean type
            setIsBoolean(true);
        } else if (ModelUtils.isNullType(p)) {
            setIsNull(true);
        } else if (ModelUtils.isAnyType(p)) {
            setIsAnyType(true);
        }
    }

    /**
     * @return basic type - no generics supported.
     */
    default String getBaseType() {
        return null;
    };

    /**
     * @return complex type that can contain type parameters - like {@code List<Items>} for Java
     */
    default String getComplexType() {
        return getBaseType();
    };

    /**
     * Recursively collect all necessary imports to include so that the type may be resolved.
     *
     * @param includeContainerTypes whether or not to include the container types in the returned imports.
     * @return all of the imports
     */
    default Set<String> getImports(boolean includeContainerTypes) {
        Set<String> imports = new HashSet<>();
        if (this.getComposedSchemas() != null) {
            CodegenComposedSchemas composed = (CodegenComposedSchemas) this.getComposedSchemas();
            List<CodegenProperty> allOfs =  composed.getAllOf() == null ? Collections.emptyList() : composed.getAllOf();
            List<CodegenProperty> oneOfs =  composed.getOneOf() == null ? Collections.emptyList() : composed.getOneOf();
            Stream<CodegenProperty> innerTypes = Stream.concat(allOfs.stream(), oneOfs.stream());
            innerTypes.flatMap(cp -> cp.getImports(includeContainerTypes).stream()).forEach(s -> imports.add(s));
        } else if (includeContainerTypes || !(this.getIsArray() || this.getIsMap())) {
            // this is our base case, add imports for referenced schemas
            // this can't be broken out as a separate if block because Typescript only generates union types as A | B
            // and would need to handle this differently
            imports.add(this.getComplexType());
            imports.add(this.getBaseType());
        }
        if (this.getItems() !=null && this.getIsArray()) {
            imports.addAll(this.getItems().getImports(includeContainerTypes));
        }
        if (this.getAdditionalProperties() != null) {
            imports.addAll(this.getAdditionalProperties().getImports(includeContainerTypes));
        }
        if (this.getVars() != null && !this.getVars().isEmpty()) {
            this.getVars().stream().flatMap(v -> v.getImports(includeContainerTypes).stream()).forEach(s -> imports.add(s));
        }
        return imports;
    }
}
