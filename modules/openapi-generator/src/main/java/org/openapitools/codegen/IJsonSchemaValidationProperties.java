package org.openapitools.codegen;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.utils.ModelUtils;

public interface IJsonSchemaValidationProperties {
    CodegenProperty getContains();

    void setContains(CodegenProperty contains);

    LinkedHashMap<String, List<String>> getDependentRequired();

    void setDependentRequired(LinkedHashMap<String, List<String>> dependentRequired);

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

    // TODO update this value to Boolean in 7.0.0
    boolean getUniqueItems();

    // TODO update this value to Boolean in 7.0.0
    void setUniqueItems(boolean uniqueItems);

    // TODO remove in 7.0.0
    Boolean getUniqueItemsBoolean();

    // TODO remove in 7.0.0
    void setUniqueItemsBoolean(Boolean uniqueItems);

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

    Map<String, CodegenProperty> getRequiredVarsMap();

    // goes from required propertyName to its CodegenProperty
    // Use Cases:
    // 1. required property is defined in properties, value is that CodegenProperty
    // 2. required property is not defined in properties, and additionalProperties is true or unset value is CodegenProperty made from empty schema
    // 3. required property is not defined in properties, and additionalProperties is schema, value is CodegenProperty made from schema
    // 4. required property is not defined in properties, and additionalProperties is false, value is null
    void setRequiredVarsMap(Map<String, CodegenProperty> requiredVarsMap);


    boolean getIsNull();

    void setIsNull(boolean isNull);

    boolean getIsVoid();

    void setIsVoid(boolean isVoid);

    boolean getHasValidation();

    void setHasValidation(boolean hasValidation);

    boolean getAdditionalPropertiesIsAnyType();

    void setAdditionalPropertiesIsAnyType(boolean additionalPropertiesIsAnyType);

    boolean getHasVars();

    void setHasVars(boolean hasVars);

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

    boolean getIsFreeFormObject();

    void setIsFreeFormObject(boolean isFreeFormObject);

    String getRef();

    void setRef(String ref);

    CodegenComposedSchemas getComposedSchemas();

    void setComposedSchemas(CodegenComposedSchemas composedSchemas);

    boolean getHasMultipleTypes();

    void setHasMultipleTypes(boolean hasMultipleTypes);

    // for when the schema is just the boolean true in a spec
    boolean getIsBooleanSchemaTrue();

    void setIsBooleanSchemaTrue(boolean isBooleanSchemaTrue);

    // for when the schema is just the boolean false in a spec
    boolean getIsBooleanSchemaFalse();

    void setIsBooleanSchemaFalse(boolean isBooleanSchemaFalse);

    boolean getSchemaIsFromAdditionalProperties();

    void setSchemaIsFromAdditionalProperties(boolean schemaIsFromAdditionalProperties);

    void setFormat(String format);

    String getFormat();

    /**
     * Syncs all the schema's type properties into the IJsonSchemaValidationProperties instance
     * for now this only supports types without format information
     * TODO: in the future move the format handling in here too
     *
     * @param p the schema which contains the type info
     */
    default void setTypeProperties(Schema p) {
        if (ModelUtils.isModelWithPropertiesOnly(p)) {
            setIsModel(true);
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
            } else if (ModelUtils.isPasswordSchema(p)) {
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
        } else if (ModelUtils.isFreeFormObject(p)) {
            setIsFreeFormObject(true);
            // TODO: remove below later after updating generators to properly use isFreeFormObject
            setIsMap(true);
        } else if (ModelUtils.isTypeObjectSchema(p)) {
            setIsMap(true);
        }
    }

    /**
     * @return basic type - no generics supported.
     */
    default String getBaseType() {
        return null;
    }

    /**
     * @return complex type that can contain type parameters - like {@code List<Items>} for Java
     */
    default String getComplexType() {
        return getBaseType();
    }

    /**
     * Recursively collect all necessary imports to include so that the type may be resolved.
     *
     * @param importContainerType whether or not to include the container types in the returned imports.
     * @param importBaseType      whether or not to include the base types in the returned imports.
     * @param featureSet          the generator feature set, used to determine if composed schemas should be added
     * @return all of the imports
     */
    default Set<String> getImports(boolean importContainerType, boolean importBaseType, FeatureSet featureSet) {
        Set<String> imports = new HashSet<>();
        if (this.getComposedSchemas() != null) {
            CodegenComposedSchemas composed = this.getComposedSchemas();
            List<CodegenProperty> allOfs = Collections.emptyList();
            List<CodegenProperty> oneOfs = Collections.emptyList();
            List<CodegenProperty> anyOfs = Collections.emptyList();
            List<CodegenProperty> nots = Collections.emptyList();
            if (composed.getAllOf() != null && featureSet.getSchemaSupportFeatures().contains(SchemaSupportFeature.allOf)) {
                allOfs = composed.getAllOf();
            }
            if (composed.getOneOf() != null && featureSet.getSchemaSupportFeatures().contains(SchemaSupportFeature.oneOf)) {
                oneOfs = composed.getOneOf();
            }
            if (composed.getAnyOf() != null && featureSet.getSchemaSupportFeatures().contains(SchemaSupportFeature.anyOf)) {
                anyOfs = composed.getAnyOf();
            }
            if (composed.getNot() != null && featureSet.getSchemaSupportFeatures().contains(SchemaSupportFeature.not)) {
                nots = Arrays.asList(composed.getNot());
            }
            Stream<CodegenProperty> innerTypes = Stream.of(
                            allOfs.stream(), anyOfs.stream(), oneOfs.stream(), nots.stream())
                    .flatMap(i -> i);
            innerTypes.flatMap(cp -> cp.getImports(importContainerType, importBaseType, featureSet).stream()).forEach(s -> imports.add(s));
        }
        // items can exist for AnyType and type array
        if (this.getItems() != null && this.getIsArray()) {
            imports.addAll(this.getItems().getImports(importContainerType, importBaseType, featureSet));
        }
        // additionalProperties can exist for AnyType and type object
        if (this.getAdditionalProperties() != null) {
            imports.addAll(this.getAdditionalProperties().getImports(importContainerType, importBaseType, featureSet));
        }
        // vars can exist for AnyType and type object
        if (this.getVars() != null && !this.getVars().isEmpty()) {
            this.getVars().stream().flatMap(v -> v.getImports(importContainerType, importBaseType, featureSet).stream()).forEach(s -> imports.add(s));
        }
        if (this.getIsArray() || this.getIsMap()) {
            if (importContainerType) {
                /*
                use-case for this complexType block:
                DefaultCodegenTest.objectQueryParamIdentifyAsObject
                DefaultCodegenTest.mapParamImportInnerObject
                */
                String complexType = this.getComplexType();
                if (complexType != null) {
                    imports.add(complexType);
                }
                /*
                use-case:
                Adding List/Map etc, Java uses this
                 */
                String baseType = this.getBaseType();
                if (importBaseType && baseType != null) {
                    imports.add(baseType);
                }
            }
        } else {
            // referenced or inline schemas
            String complexType = this.getComplexType();
            if (complexType != null) {
                imports.add(complexType);
            }
            String baseType = this.getBaseType();
            if (importBaseType && baseType != null) {
                imports.add(baseType);
            }
            return imports;
        }
        return imports;
    }
}
