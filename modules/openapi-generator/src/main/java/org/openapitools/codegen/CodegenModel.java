/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.v3.oas.models.ExternalDocumentation;

import java.util.*;

/**
 * CodegenModel represents a schema object in a OpenAPI document.
 */
@JsonIgnoreProperties({"parentModel", "interfaceModels"})
public class CodegenModel implements IJsonSchemaValidationProperties {
    // The parent model name from the schemas. The parent is determined by inspecting the allOf, anyOf and
    // oneOf attributes in the OAS. First codegen inspects 'allOf', then 'anyOf', then 'oneOf'.
    // If there are multiple object references in the attribute ('allOf', 'anyOf', 'oneOf'), and one of the
    // object is a discriminator, that object is set as the parent. If no discriminator is specified,
    // codegen returns the first one in the list, i.e. there is no obvious parent in the OpenAPI specification.
    // When possible, the mustache templates should use 'allParents' to handle multiple parents.
    public String parent, parentSchema;
    public List<String> interfaces;
    // The list of parent model name from the schemas. In order of preference, the parent is obtained
    // from the 'allOf' attribute, then 'anyOf', and finally 'oneOf'.
    public List<String> allParents;

    // References to parent and interface CodegenModels. Only set when code generator supports inheritance.
    public CodegenModel parentModel;
    public List<CodegenModel> interfaceModels;
    public List<CodegenModel> children;

    // anyOf, oneOf, allOf
    public Set<String> anyOf = new TreeSet<>();
    public Set<String> oneOf = new TreeSet<>();
    public Set<String> allOf = new TreeSet<>();

    // The schema name as written in the OpenAPI document.
    public String name;
    // The language-specific name of the class that implements this schema.
    // The name of the class is derived from the OpenAPI schema name with formatting rules applied.
    // The classname is derived from the OpenAPI schema name, with sanitization and escaping rules applied.
    public String classname;
    // The value of the 'title' attribute in the OpenAPI document.
    public String title;
    public String description, classVarName, modelJson, dataType, xmlPrefix, xmlNamespace, xmlName;
    public String classFilename; // store the class file name, mainly used for import
    public String unescapedDescription;
    public CodegenDiscriminator discriminator;
    public String defaultValue;
    public String arrayModelType;
    public boolean isAlias; // Is this effectively an alias of another simple type
    public boolean isString, isInteger, isLong, isNumber, isNumeric, isFloat, isDouble, isDate, isDateTime, isShort, isUnboundedInteger, isBoolean;
    private boolean additionalPropertiesIsAnyType;
    public List<CodegenProperty> vars = new ArrayList<>(); // all properties (without parent's properties)
    public List<CodegenProperty> allVars = new ArrayList<>(); // all properties (with parent's properties)
    public List<CodegenProperty> requiredVars = new ArrayList<>(); // a list of required properties
    public List<CodegenProperty> optionalVars = new ArrayList<>(); // a list of optional properties
    public List<CodegenProperty> readOnlyVars = new ArrayList<>(); // a list of read-only properties
    public List<CodegenProperty> readWriteVars = new ArrayList<>(); // a list of properties for read, write
    public List<CodegenProperty> parentVars = new ArrayList<>();
    public Map<String, Object> allowableValues;

    // Sorted sets of required parameters.
    public Set<String> mandatory = new TreeSet<>(); // without parent's required properties
    public Set<String> allMandatory = new TreeSet<>(); // with parent's required properties

    public Set<String> imports = new TreeSet<>();
    public boolean hasVars, emptyVars, hasMoreModels, hasEnums, isEnum, hasValidation;
    /**
     * Indicates the OAS schema specifies "nullable: true".
     */
    public boolean isNullable;
    /**
     * Indicates the type has at least one required property.
     */
    public boolean hasRequired;
    /**
     * Indicates the type has at least one optional property.
     */
    public boolean hasOptional;
    public boolean isArray;
    public boolean hasChildren;
    public boolean isMap;
    public boolean isNull;
    /**
     * Indicates the OAS schema specifies "deprecated: true".
     */
    public boolean isDeprecated;
    public boolean hasOnlyReadOnly = true; // true if all properties are read-only
    public ExternalDocumentation externalDocumentation;

    public Map<String, Object> vendorExtensions = new HashMap<>();
    private CodegenComposedSchemas composedSchemas;
    private boolean hasMultipleTypes = false;

    /**
     * The type of the value for the additionalProperties keyword in the OAS document.
     * Used in map like objects, including composed schemas.
     *
     * In most programming languages, the additional (undeclared) properties are stored
     * in a map data structure, such as HashMap in Java, map in golang, or a dict in Python.
     * There are multiple ways to implement the additionalProperties keyword, depending
     * on the programming language and mustache template.
     * One way is to use class inheritance. For example in the generated Java code, the
     * generated model class may extend from HashMap to store the additional properties.
     * In that case 'CodegenModel.parent' is set to represent the class hierarchy.
     * Another way is to use CodegenModel.additionalPropertiesType. A code generator
     * such as Python does not use class inheritance to model additional properties.
     *
     * For example, in the OAS schema below, the schema has a declared 'id' property
     * and additional, undeclared properties of type 'integer' are allowed.
     *
     * type: object
     * properties:
     *   id:
     *     type: integer
     * additionalProperties:
     *   type: integer
     *
     */
    public String additionalPropertiesType;

    /**
     * True if additionalProperties is set to true (boolean value)
     */
    public boolean isAdditionalPropertiesTrue;

    private Integer maxProperties;
    private Integer minProperties;
    private boolean uniqueItems;
    private Integer maxItems;
    private Integer minItems;
    private Integer maxLength;
    private Integer minLength;
    private boolean exclusiveMinimum;
    private boolean exclusiveMaximum;
    private String minimum;
    private String maximum;
    private String pattern;
    private Number multipleOf;
    private CodegenProperty items;
    private CodegenProperty additionalProperties;
    private boolean isModel;
    private boolean hasRequiredVars;
    private boolean hasDiscriminatorWithNonEmptyMapping;
    private boolean isAnyType;

    public String getAdditionalPropertiesType() {
        return additionalPropertiesType;
    }

    public void setAdditionalPropertiesType(String additionalPropertiesType) {
        this.additionalPropertiesType = additionalPropertiesType;
    }

    public Set<String> getAllMandatory() {
        return allMandatory;
    }

    public void setAllMandatory(Set<String> allMandatory) {
        this.allMandatory = allMandatory;
    }

    public List<String> getAllParents() {
        return allParents;
    }

    public void setAllParents(List<String> allParents) {
        this.allParents = allParents;
    }

    public List<CodegenProperty> getAllVars() {
        return allVars;
    }

    public void setAllVars(List<CodegenProperty> allVars) {
        this.allVars = allVars;
    }

    public Map<String, Object> getAllowableValues() {
        return allowableValues;
    }

    public void setAllowableValues(Map<String, Object> allowableValues) {
        this.allowableValues = allowableValues;
    }

    public String getArrayModelType() {
        return arrayModelType;
    }

    public void setArrayModelType(String arrayModelType) {
        this.arrayModelType = arrayModelType;
    }

    public List<CodegenModel> getChildren() {
        return children;
    }

    public void setChildren(List<CodegenModel> children) {
        this.children = children;
    }

    public String getClassFilename() {
        return classFilename;
    }

    public void setClassFilename(String classFilename) {
        this.classFilename = classFilename;
    }

    public String getClassVarName() {
        return classVarName;
    }

    public void setClassVarName(String classVarName) {
        this.classVarName = classVarName;
    }

    /**
     * Return true if the classname property is sanitized, false if it is the same as the OpenAPI schema name.
     * The OpenAPI schema name may be any valid JSON schema name, including non-ASCII characters.
     * The name of the class may have to be sanitized with character escaping.
     *
     * @return true if the classname property is sanitized
     */
    public boolean getIsClassnameSanitized() {
        return !classname.equals(name);
    }

    public String getClassname() {
        return classname;
    }

    public void setClassname(String classname) {
        this.classname = classname;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Returns the discriminator for this schema object, or null if no discriminator has been specified.
     *
     * The list of all possible schema discriminator mapping values is obtained
     * from explicit discriminator mapping values in the OpenAPI document, and from
     * inherited discriminators through oneOf, allOf, anyOf.
     * For example, a discriminator may be defined in a 'Pet' schema as shown below.
     * The Dog and Cat schemas inherit the discriminator through the allOf reference.
     * In the 'Pet' schema, the supported discriminator mapping values for the
     * 'objectType' properties are 'Dog' and 'Cat'.
     * The allowed discriminator mapping value for the Dog schema is 'Dog'.
     * The allowed discriminator mapping value for the Cat schema is 'Dog'.
     *
     * Pet:
     *   type: object
     *   discriminator:
     *     propertyName: objectType
     *   required:
     *     - objectType
     *   properties:
     *     objectType:
     *     type: string
     * Dog:
     *   allOf:
     *   - $ref: '#/components/schemas/Pet'
     *   - type: object
     *     properties:
     *       p1:
     *         type: string
     * Cat:
     *   allOf:
     *   - $ref: '#/components/schemas/Pet'
     *   - type: object
     *     properties:
     *       p2:
     *         type: string
     *
     * @return the discriminator.
     */
    public CodegenDiscriminator getDiscriminator() {
        return discriminator;
    }

    public void setDiscriminator(CodegenDiscriminator discriminator) {
        this.discriminator = discriminator;
        if (discriminator != null && !discriminator.getMappedModels().isEmpty()) {
            this.hasDiscriminatorWithNonEmptyMapping = true;
        }
    }

    /**
     * Returns the name of the discriminator property for this schema in the OpenAPI document.
     * In the OpenAPI document, the discriminator may be specified in the local schema or
     * it may be inherited, such as through a 'allOf' schema which references another schema
     * that has a discriminator, recursively.
     *
     * @return the name of the discriminator property.
     */
    public String getDiscriminatorName() {
        return discriminator == null ? null : discriminator.getPropertyName();
    }

    public ExternalDocumentation getExternalDocumentation() {
        return externalDocumentation;
    }

    public void setExternalDocumentation(ExternalDocumentation externalDocumentation) {
        this.externalDocumentation = externalDocumentation;
    }

    public Set<String> getImports() {
        return imports;
    }

    public void setImports(Set<String> imports) {
        this.imports = imports;
    }

    public List<CodegenModel> getInterfaceModels() {
        return interfaceModels;
    }

    public void setInterfaceModels(List<CodegenModel> interfaceModels) {
        this.interfaceModels = interfaceModels;
    }

    public List<String> getInterfaces() {
        return interfaces;
    }

    public void setInterfaces(List<String> interfaces) {
        this.interfaces = interfaces;
    }

    public Set<String> getMandatory() {
        return mandatory;
    }

    public void setMandatory(Set<String> mandatory) {
        this.mandatory = mandatory;
    }

    public String getModelJson() {
        return modelJson;
    }

    public void setModelJson(String modelJson) {
        this.modelJson = modelJson;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<CodegenProperty> getOptionalVars() {
        return optionalVars;
    }

    public void setOptionalVars(List<CodegenProperty> optionalVars) {
        this.optionalVars = optionalVars;
    }

    public String getParent() {
        return parent;
    }

    public void setParent(String parent) {
        this.parent = parent;
    }

    public CodegenModel getParentModel() {
        return parentModel;
    }

    public void setParentModel(CodegenModel parentModel) {
        this.parentModel = parentModel;
    }

    public String getParentSchema() {
        return parentSchema;
    }

    public void setParentSchema(String parentSchema) {
        this.parentSchema = parentSchema;
    }

    public List<CodegenProperty> getParentVars() {
        return parentVars;
    }

    public void setParentVars(List<CodegenProperty> parentVars) {
        this.parentVars = parentVars;
    }

    @Override
    public String getPattern() {
        return pattern;
    }

    @Override
    public void setPattern(String pattern) {
        this.pattern = pattern;
    }

    @Override
    public String getMaximum() {
        return maximum;
    }

    @Override
    public void setMaximum(String maximum) {
        this.maximum = maximum;
    }

    @Override
    public String getMinimum() {
        return minimum;
    }

    @Override
    public void setMinimum(String minimum) {
        this.minimum = minimum;
    }

    @Override
    public boolean getExclusiveMaximum() {
        return exclusiveMaximum;
    }

    @Override
    public void setExclusiveMaximum(boolean exclusiveMaximum) {
        this.exclusiveMaximum = exclusiveMaximum;
    }

    @Override
    public boolean getExclusiveMinimum() {
        return exclusiveMinimum;
    }

    @Override
    public void setExclusiveMinimum(boolean exclusiveMinimum) {
        this.exclusiveMinimum = exclusiveMinimum;
    }

    @Override
    public Integer getMinLength() {
        return minLength;
    }

    @Override
    public void setMinLength(Integer minLength) {
        this.minLength = minLength;
    }

    @Override
    public Integer getMaxLength() {
        return maxLength;
    }

    @Override
    public void setMaxLength(Integer maxLength) {
        this.maxLength = maxLength;
    }

    @Override
    public Integer getMinItems() {
        return minItems;
    }

    @Override
    public void setMinItems(Integer minItems) {
        this.minItems = minItems;
    }

    @Override
    public Integer getMaxItems() {
        return maxItems;
    }

    @Override
    public void setMaxItems(Integer maxItems) {
        this.maxItems = maxItems;
    }

    @Override
    public boolean getUniqueItems() {
        return uniqueItems;
    }

    @Override
    public void setUniqueItems(boolean uniqueItems) {
        this.uniqueItems = uniqueItems;
    }

    @Override
    public Integer getMinProperties() {
        return minProperties;
    }

    @Override
    public void setMinProperties(Integer minProperties) {
        this.minProperties = minProperties;
    }

    @Override
    public Integer getMaxProperties() {
        return maxProperties;
    }

    @Override
    public void setMaxProperties(Integer maxProperties) {
        this.maxProperties = maxProperties;
    }

    @Override
    public Number getMultipleOf() {
        return multipleOf;
    }

    @Override
    public void setMultipleOf(Number multipleOf) {
        this.multipleOf = multipleOf;
    }

    @Override
    public CodegenProperty getItems() {
        return items;
    }

    @Override
    public void setItems(CodegenProperty items) {
        this.items = items;
    }

    @Override
    public boolean getIsModel() { return isModel; }

    @Override
    public void setIsModel(boolean isModel)  {
        this.isModel = isModel;
    }

    @Override
    public boolean getIsDate() { return isDate; }

    @Override
    public void setIsDate(boolean isDate)   {
        this.isDate = isDate;
    }

    @Override
    public boolean getIsDateTime() { return isDateTime; }

    @Override
    public void setIsDateTime(boolean isDateTime)   {
        this.isDateTime = isDateTime;
    }

    @Override
    public boolean getIsMap() { return isMap; }

    @Override
    public void setIsMap(boolean isMap)  {
        this.isMap = isMap;
    }

    @Override
    public boolean getIsArray() { return isArray; }

    @Override
    public void setIsArray(boolean isArray)  {
        this.isArray = isArray;
    }

    @Override
    public boolean getIsShort() { return isShort; }

    @Override
    public void setIsShort(boolean isShort)   {
        this.isShort = isShort;
    }

    @Override
    public boolean getIsBoolean() { return isBoolean; }

    @Override
    public void setIsBoolean(boolean isBoolean)   {
        this.isBoolean= isBoolean;
    }

    @Override
    public boolean getIsUnboundedInteger() { return isUnboundedInteger; }

    @Override
    public void setIsUnboundedInteger(boolean isUnboundedInteger)   {
        this.isUnboundedInteger = isUnboundedInteger;
    }

    @Override
    public CodegenProperty getAdditionalProperties() { return additionalProperties; }

    @Override
    public void setAdditionalProperties(CodegenProperty additionalProperties)  {
        this.additionalProperties = additionalProperties;
    }

    @Override
    public boolean getHasValidation() { return hasValidation; }

    @Override
    public void setHasValidation(boolean hasValidation) { this.hasValidation = hasValidation; }

    public List<CodegenProperty> getReadOnlyVars() {
        return readOnlyVars;
    }

    public void setReadOnlyVars(List<CodegenProperty> readOnlyVars) {
        this.readOnlyVars = readOnlyVars;
    }

    public List<CodegenProperty> getReadWriteVars() {
        return readWriteVars;
    }

    public void setReadWriteVars(List<CodegenProperty> readWriteVars) {
        this.readWriteVars = readWriteVars;
    }

    @Override
    public List<CodegenProperty> getRequiredVars() {
        return requiredVars;
    }

    @Override
    public void setRequiredVars(List<CodegenProperty> requiredVars) {
        this.requiredVars = requiredVars;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getUnescapedDescription() {
        return unescapedDescription;
    }

    public void setUnescapedDescription(String unescapedDescription) {
        this.unescapedDescription = unescapedDescription;
    }

    @Override
    public List<CodegenProperty> getVars() {
        return vars;
    }

    @Override
    public void setVars(List<CodegenProperty> vars) {
        this.vars = vars;
    }

    public Map<String, Object> getVendorExtensions() {
        return vendorExtensions;
    }

    public void setVendorExtensions(Map<String, Object> vendorExtensions) {
        this.vendorExtensions = vendorExtensions;
    }

    public String getXmlName() {
        return xmlName;
    }

    public void setXmlName(String xmlName) {
        this.xmlName = xmlName;
    }

    public String getXmlNamespace() {
        return xmlNamespace;
    }

    public void setXmlNamespace(String xmlNamespace) {
        this.xmlNamespace = xmlNamespace;
    }

    public String getXmlPrefix() {
        return xmlPrefix;
    }

    public void setXmlPrefix(String xmlPrefix) {
        this.xmlPrefix = xmlPrefix;
    }

    @Override
    public boolean getIsNull() {
        return isNull;
    }

    @Override
    public void setIsNull(boolean isNull) {
        this.isNull = isNull;
    }

    @Override
    public boolean getAdditionalPropertiesIsAnyType() {
        return additionalPropertiesIsAnyType;
    }

    @Override
    public void setAdditionalPropertiesIsAnyType(boolean additionalPropertiesIsAnyType) {
        this.additionalPropertiesIsAnyType = additionalPropertiesIsAnyType;
    }

    @Override
    public boolean getHasVars() {
        return this.hasVars;
    }

    @Override
    public void setHasVars(boolean hasVars) {
        this.hasVars = hasVars;
    }

    @Override
    public boolean getHasRequired() {
        return this.hasRequired;
    }

    @Override
    public void setHasRequired(boolean hasRequired) {
        this.hasRequired = hasRequired;
    }

    @Override
    public boolean getHasDiscriminatorWithNonEmptyMapping() { return hasDiscriminatorWithNonEmptyMapping; };

    @Override
    public void setHasDiscriminatorWithNonEmptyMapping(boolean hasDiscriminatorWithNonEmptyMapping) {
        this.hasDiscriminatorWithNonEmptyMapping = hasDiscriminatorWithNonEmptyMapping;
    }

    @Override
    public boolean getIsString() { return isString; }

    @Override
    public void setIsString(boolean isString)  {
        this.isString = isString;
    }

    @Override
    public boolean getIsNumber() { return isNumber; }

    @Override
    public void setIsNumber(boolean isNumber)  {
        this.isNumber = isNumber;
    }

    @Override
    public boolean getIsAnyType() { return isAnyType; }

    @Override
    public void setIsAnyType(boolean isAnyType)  {
        this.isAnyType = isAnyType;
    }

    @Override
    public void setComposedSchemas(CodegenComposedSchemas composedSchemas) {
        this.composedSchemas = composedSchemas;
    }

    @Override
    public CodegenComposedSchemas getComposedSchemas() {
        return composedSchemas;
    }

    @Override
    public boolean getHasMultipleTypes() {return hasMultipleTypes; }

    @Override
    public void setHasMultipleTypes(boolean hasMultipleTypes) { this.hasMultipleTypes = hasMultipleTypes; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CodegenModel)) return false;
        CodegenModel that = (CodegenModel) o;
        return isAlias == that.isAlias &&
                isString == that.isString &&
                isInteger == that.isInteger &&
                isShort == that.isShort &&
                isLong == that.isLong &&
                isUnboundedInteger == that.isUnboundedInteger &&
                isBoolean == that.isBoolean &&
                isNumber == that.isNumber &&
                isNumeric == that.isNumeric &&
                isFloat == that.isFloat &&
                isDouble == that.isDouble &&
                isDate == that.isDate &&
                isDateTime == that.isDateTime &&
                hasVars == that.hasVars &&
                emptyVars == that.emptyVars &&
                hasMoreModels == that.hasMoreModels &&
                hasEnums == that.hasEnums &&
                isEnum == that.isEnum &&
                isNullable == that.isNullable &&
                hasRequired == that.hasRequired &&
                hasOptional == that.hasOptional &&
                isArray == that.isArray &&
                hasChildren == that.hasChildren &&
                isMap == that.isMap &&
                isDeprecated == that.isDeprecated &&
                hasOnlyReadOnly == that.hasOnlyReadOnly &&
                isNull == that.isNull &&
                hasValidation == that.hasValidation &&
                hasMultipleTypes == that.getHasMultipleTypes() &&
                hasDiscriminatorWithNonEmptyMapping == that.getHasDiscriminatorWithNonEmptyMapping() &&
                getIsAnyType() == that.getIsAnyType() &&
                getAdditionalPropertiesIsAnyType() == that.getAdditionalPropertiesIsAnyType() &&
                getUniqueItems() == that.getUniqueItems() &&
                getExclusiveMinimum() == that.getExclusiveMinimum() &&
                getExclusiveMaximum() == that.getExclusiveMaximum() &&
                Objects.equals(composedSchemas, that.composedSchemas) &&
                Objects.equals(parent, that.parent) &&
                Objects.equals(parentSchema, that.parentSchema) &&
                Objects.equals(interfaces, that.interfaces) &&
                Objects.equals(allParents, that.allParents) &&
                Objects.equals(parentModel, that.parentModel) &&
                Objects.equals(interfaceModels, that.interfaceModels) &&
                Objects.equals(children, that.children) &&
                Objects.equals(anyOf, that.anyOf) &&
                Objects.equals(oneOf, that.oneOf) &&
                Objects.equals(allOf, that.allOf) &&
                Objects.equals(name, that.name) &&
                Objects.equals(classname, that.classname) &&
                Objects.equals(title, that.title) &&
                Objects.equals(description, that.description) &&
                Objects.equals(classVarName, that.classVarName) &&
                Objects.equals(modelJson, that.modelJson) &&
                Objects.equals(dataType, that.dataType) &&
                Objects.equals(xmlPrefix, that.xmlPrefix) &&
                Objects.equals(xmlNamespace, that.xmlNamespace) &&
                Objects.equals(xmlName, that.xmlName) &&
                Objects.equals(classFilename, that.classFilename) &&
                Objects.equals(unescapedDescription, that.unescapedDescription) &&
                Objects.equals(discriminator, that.discriminator) &&
                Objects.equals(defaultValue, that.defaultValue) &&
                Objects.equals(arrayModelType, that.arrayModelType) &&
                Objects.equals(vars, that.vars) &&
                Objects.equals(allVars, that.allVars) &&
                Objects.equals(requiredVars, that.requiredVars) &&
                Objects.equals(optionalVars, that.optionalVars) &&
                Objects.equals(readOnlyVars, that.readOnlyVars) &&
                Objects.equals(readWriteVars, that.readWriteVars) &&
                Objects.equals(parentVars, that.parentVars) &&
                Objects.equals(allowableValues, that.allowableValues) &&
                Objects.equals(mandatory, that.mandatory) &&
                Objects.equals(allMandatory, that.allMandatory) &&
                Objects.equals(imports, that.imports) &&
                Objects.equals(externalDocumentation, that.externalDocumentation) &&
                Objects.equals(vendorExtensions, that.vendorExtensions) &&
                Objects.equals(additionalPropertiesType, that.additionalPropertiesType) &&
                Objects.equals(getMaxProperties(), that.getMaxProperties()) &&
                Objects.equals(getMinProperties(), that.getMinProperties()) &&
                Objects.equals(getMaxItems(), that.getMaxItems()) &&
                Objects.equals(getMinItems(), that.getMinItems()) &&
                Objects.equals(getMaxLength(), that.getMaxLength()) &&
                Objects.equals(getMinLength(), that.getMinLength()) &&
                Objects.equals(getMinimum(), that.getMinimum()) &&
                Objects.equals(getMaximum(), that.getMaximum()) &&
                Objects.equals(getPattern(), that.getPattern()) &&
                Objects.equals(getItems(), that.getItems()) &&
                Objects.equals(getAdditionalProperties(), that.getAdditionalProperties()) &&
                Objects.equals(getIsModel(), that.getIsModel()) &&
                Objects.equals(getMultipleOf(), that.getMultipleOf());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getParent(), getParentSchema(), getInterfaces(), getAllParents(), getParentModel(),
                getInterfaceModels(), getChildren(), anyOf, oneOf, allOf, getName(), getClassname(), getTitle(),
                getDescription(), getClassVarName(), getModelJson(), getDataType(), getXmlPrefix(), getXmlNamespace(),
                getXmlName(), getClassFilename(), getUnescapedDescription(), getDiscriminator(), getDefaultValue(),
                getArrayModelType(), isAlias, isString, isInteger, isLong, isNumber, isNumeric, isFloat, isDouble,
                isDate, isDateTime, isNull, hasValidation, isShort, isUnboundedInteger, isBoolean,
                getVars(), getAllVars(), getRequiredVars(), getOptionalVars(), getReadOnlyVars(), getReadWriteVars(),
                getParentVars(), getAllowableValues(), getMandatory(), getAllMandatory(), getImports(), hasVars,
                isEmptyVars(), hasMoreModels, hasEnums, isEnum, isNullable, hasRequired, hasOptional, isArray,
                hasChildren, isMap, isDeprecated, hasOnlyReadOnly, getExternalDocumentation(), getVendorExtensions(),
                getAdditionalPropertiesType(), getMaxProperties(), getMinProperties(), getUniqueItems(), getMaxItems(),
                getMinItems(), getMaxLength(), getMinLength(), getExclusiveMinimum(), getExclusiveMaximum(), getMinimum(),
                getMaximum(), getPattern(), getMultipleOf(), getItems(), getAdditionalProperties(), getIsModel(),
                getAdditionalPropertiesIsAnyType(), hasDiscriminatorWithNonEmptyMapping,
                isAnyType, getComposedSchemas(), hasMultipleTypes);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("CodegenModel{");
        sb.append("parent='").append(parent).append('\'');
        sb.append(", parentSchema='").append(parentSchema).append('\'');
        sb.append(", interfaces=").append(interfaces);
        sb.append(", allParents=").append(allParents);
        sb.append(", parentModel=").append(parentModel);
        sb.append(", interfaceModels=").append(interfaceModels);
        sb.append(", children=").append(children);
        sb.append(", anyOf=").append(anyOf);
        sb.append(", oneOf=").append(oneOf);
        sb.append(", allOf=").append(allOf);
        sb.append(", name='").append(name).append('\'');
        sb.append(", classname='").append(classname).append('\'');
        sb.append(", title='").append(title).append('\'');
        sb.append(", description='").append(description).append('\'');
        sb.append(", classVarName='").append(classVarName).append('\'');
        sb.append(", modelJson='").append(modelJson).append('\'');
        sb.append(", dataType='").append(dataType).append('\'');
        sb.append(", xmlPrefix='").append(xmlPrefix).append('\'');
        sb.append(", xmlNamespace='").append(xmlNamespace).append('\'');
        sb.append(", xmlName='").append(xmlName).append('\'');
        sb.append(", classFilename='").append(classFilename).append('\'');
        sb.append(", unescapedDescription='").append(unescapedDescription).append('\'');
        sb.append(", discriminator=").append(discriminator);
        sb.append(", defaultValue='").append(defaultValue).append('\'');
        sb.append(", arrayModelType='").append(arrayModelType).append('\'');
        sb.append(", isAlias=").append(isAlias);
        sb.append(", isString=").append(isString);
        sb.append(", isInteger=").append(isInteger);
        sb.append(", isShort=").append(isShort);
        sb.append(", isLong=").append(isLong);
        sb.append(", isUnboundedInteger=").append(isUnboundedInteger);
        sb.append(", isBoolean=").append(isBoolean);
        sb.append(", isNumber=").append(isNumber);
        sb.append(", isNumeric=").append(isNumeric);
        sb.append(", isFloat=").append(isFloat);
        sb.append(", isDouble=").append(isDouble);
        sb.append(", isDate=").append(isDate);
        sb.append(", isDateTime=").append(isDateTime);
        sb.append(", vars=").append(vars);
        sb.append(", allVars=").append(allVars);
        sb.append(", requiredVars=").append(requiredVars);
        sb.append(", optionalVars=").append(optionalVars);
        sb.append(", readOnlyVars=").append(readOnlyVars);
        sb.append(", readWriteVars=").append(readWriteVars);
        sb.append(", parentVars=").append(parentVars);
        sb.append(", allowableValues=").append(allowableValues);
        sb.append(", mandatory=").append(mandatory);
        sb.append(", allMandatory=").append(allMandatory);
        sb.append(", imports=").append(imports);
        sb.append(", hasVars=").append(hasVars);
        sb.append(", emptyVars=").append(emptyVars);
        sb.append(", hasMoreModels=").append(hasMoreModels);
        sb.append(", hasEnums=").append(hasEnums);
        sb.append(", isEnum=").append(isEnum);
        sb.append(", isNullable=").append(isNullable);
        sb.append(", hasRequired=").append(hasRequired);
        sb.append(", hasOptional=").append(hasOptional);
        sb.append(", isArray=").append(isArray);
        sb.append(", hasChildren=").append(hasChildren);
        sb.append(", isMap=").append(isMap);
        sb.append(", isDeprecated=").append(isDeprecated);
        sb.append(", hasOnlyReadOnly=").append(hasOnlyReadOnly);
        sb.append(", externalDocumentation=").append(externalDocumentation);
        sb.append(", vendorExtensions=").append(vendorExtensions);
        sb.append(", additionalPropertiesType='").append(additionalPropertiesType).append('\'');
        sb.append(", maxProperties=").append(maxProperties);
        sb.append(", minProperties=").append(minProperties);
        sb.append(", uniqueItems=").append(uniqueItems);
        sb.append(", maxItems=").append(maxItems);
        sb.append(", minItems=").append(minItems);
        sb.append(", maxLength=").append(maxLength);
        sb.append(", minLength=").append(minLength);
        sb.append(", exclusiveMinimum=").append(exclusiveMinimum);
        sb.append(", exclusiveMaximum=").append(exclusiveMaximum);
        sb.append(", minimum='").append(minimum).append('\'');
        sb.append(", maximum='").append(maximum).append('\'');
        sb.append(", pattern='").append(pattern).append('\'');
        sb.append(", multipleOf='").append(multipleOf).append('\'');
        sb.append(", items='").append(items).append('\'');
        sb.append(", additionalProperties='").append(additionalProperties).append('\'');
        sb.append(", isModel='").append(isModel).append('\'');
        sb.append(", isNull='").append(isNull);
        sb.append(", hasValidation='").append(hasValidation);
        sb.append(", getAdditionalPropertiesIsAnyType=").append(getAdditionalPropertiesIsAnyType());
        sb.append(", getHasDiscriminatorWithNonEmptyMapping=").append(hasDiscriminatorWithNonEmptyMapping);
        sb.append(", getIsAnyType=").append(getIsAnyType());
        sb.append(", composedSchemas=").append(composedSchemas);
        sb.append(", hasMultipleTypes=").append(hasMultipleTypes);
        sb.append('}');
        return sb.toString();
    }

    public void addDiscriminatorMappedModelsImports(){
        if (discriminator == null || discriminator.getMappedModels() == null) {
            return;
        }
        for (CodegenDiscriminator.MappedModel mm : discriminator.getMappedModels()) {
            if (!"".equals(mm.getModelName())) {
                imports.add(mm.getModelName());
            }
        }
    }

    public boolean isEmptyVars() {
        return emptyVars;
    }

    public void setEmptyVars(boolean emptyVars) {
        this.emptyVars = emptyVars;
    }

    /**
     * Remove duplicated properties in all variable list
     */
    public void removeAllDuplicatedProperty() {
        // remove duplicated properties
        vars = removeDuplicatedProperty(vars);
        optionalVars = removeDuplicatedProperty(optionalVars);
        requiredVars = removeDuplicatedProperty(requiredVars);
        parentVars = removeDuplicatedProperty(parentVars);
        allVars = removeDuplicatedProperty(allVars);
        readOnlyVars = removeDuplicatedProperty(readOnlyVars);
        readWriteVars = removeDuplicatedProperty(readWriteVars);
    }

    private List<CodegenProperty> removeDuplicatedProperty(List<CodegenProperty> vars) {
        // clone the list first
        List<CodegenProperty> newList = new ArrayList<>();
        for (CodegenProperty cp : vars) {
            newList.add(cp.clone());
        }

        Set<String> propertyNames = new TreeSet<>();
        Set<String> duplicatedNames = new TreeSet<>();

        ListIterator<CodegenProperty> iterator = newList.listIterator();
        while (iterator.hasNext()) {
            CodegenProperty element = iterator.next();

            if (propertyNames.contains(element.baseName)) {
                duplicatedNames.add(element.baseName);
                iterator.remove();
            } else {
                propertyNames.add(element.baseName);
            }
        }

        return newList;
    }

    /**
     * Remove self reference import
     */
    public void removeSelfReferenceImport() {
        for (CodegenProperty cp : allVars) {
            if (cp == null) {
                // TODO cp shouldn't be null. Show a warning message instead
            } else {
                // detect self import
                if (this.classname.equalsIgnoreCase(cp.dataType) ||
                        (cp.isContainer && cp.items != null && this.classname.equalsIgnoreCase(cp.items.dataType))) {
                    this.imports.remove(this.classname); // remove self import
                    cp.isSelfReference = true;
                }
            }
        }
    }
}
