/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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

@JsonIgnoreProperties({"parentModel", "interfaceModels"})
public class CodegenModel implements IJsonSchemaValidationProperties {
    public String parent, parentSchema;
    public List<String> interfaces;
    public List<String> allParents;

    // References to parent and interface CodegenModels. Only set when code generator supports inheritance.
    public CodegenModel parentModel;
    public List<CodegenModel> interfaceModels;
    public List<CodegenModel> children;

    // anyOf, oneOf, allOf
    public Set<String> anyOf = new TreeSet<String>();
    public Set<String> oneOf = new TreeSet<String>();
    public Set<String> allOf = new TreeSet<String>();

    public String name, classname, title, description, classVarName, modelJson, dataType, xmlPrefix, xmlNamespace, xmlName;
    public String classFilename; // store the class file name, mainly used for import
    public String unescapedDescription;
    public CodegenDiscriminator discriminator;
    public String defaultValue;
    public String arrayModelType;
    public Set<String> circularReferences = new TreeSet<String>(); // store all classes cross referencing this
    public boolean isAlias; // Is this effectively an alias of another simple type
    public boolean isString, isInteger, isLong, isNumber, isNumeric, isFloat, isDouble;
    public List<CodegenProperty> vars = new ArrayList<CodegenProperty>(); // all properties (without parent's properties)
    public List<CodegenProperty> allVars = new ArrayList<CodegenProperty>(); // all properties (with parent's properties)
    public List<CodegenProperty> requiredVars = new ArrayList<CodegenProperty>(); // a list of required properties
    public List<CodegenProperty> optionalVars = new ArrayList<CodegenProperty>(); // a list of optional properties
    public List<CodegenProperty> readOnlyVars = new ArrayList<CodegenProperty>(); // a list of read-only properties
    public List<CodegenProperty> readWriteVars = new ArrayList<CodegenProperty>(); // a list of properties for read, write
    public List<CodegenProperty> parentVars = new ArrayList<CodegenProperty>();
    public Map<String, Object> allowableValues;

    // Sorted sets of required parameters.
    public Set<String> mandatory = new TreeSet<String>(); // without parent's required properties
    public Set<String> allMandatory = new TreeSet<String>(); // with parent's required properties

    public Set<String> imports = new TreeSet<String>();
    public boolean hasVars, emptyVars, hasMoreModels, hasEnums, isEnum, isNullable, hasRequired, hasOptional, isArrayModel, hasChildren, isMapModel;
    public boolean hasOnlyReadOnly = true; // true if all properties are read-only
    public ExternalDocumentation externalDocumentation;

    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();

    //The type of the value from additional properties. Used in map like objects.
    public String additionalPropertiesType;

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

    public Set<String> getCircularReferences() {
        return circularReferences;
    }

    public void setCircularReferences(Set<String> circularReferences) {
        this.circularReferences = circularReferences;
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

    public CodegenDiscriminator getDiscriminator() {
        return discriminator;
    }

    public void setDiscriminator(CodegenDiscriminator discriminator) {
        this.discriminator = discriminator;
    }

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

    public List<CodegenProperty> getRequiredVars() {
        return requiredVars;
    }

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

    public List<CodegenProperty> getVars() {
        return vars;
    }

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
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CodegenModel)) return false;
        CodegenModel that = (CodegenModel) o;
        return isAlias == that.isAlias &&
                isString == that.isString &&
                isInteger == that.isInteger &&
                isLong == that.isLong &&
                isNumber == that.isNumber &&
                isNumeric == that.isNumeric &&
                isFloat == that.isFloat &&
                isDouble == that.isDouble &&
                hasVars == that.hasVars &&
                emptyVars == that.emptyVars &&
                hasMoreModels == that.hasMoreModels &&
                hasEnums == that.hasEnums &&
                isEnum == that.isEnum &&
                isNullable == that.isNullable &&
                hasRequired == that.hasRequired &&
                hasOptional == that.hasOptional &&
                isArrayModel == that.isArrayModel &&
                hasChildren == that.hasChildren &&
                isMapModel == that.isMapModel &&
                hasOnlyReadOnly == that.hasOnlyReadOnly &&
                getUniqueItems() == that.getUniqueItems() &&
                getExclusiveMinimum() == that.getExclusiveMinimum() &&
                getExclusiveMaximum() == that.getExclusiveMaximum() &&
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
                Objects.equals(circularReferences, that.circularReferences) &&
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
                Objects.equals(getPattern(), that.getPattern());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getParent(), getParentSchema(), getInterfaces(), getAllParents(), getParentModel(),
                getInterfaceModels(), getChildren(), anyOf, oneOf, allOf, getName(), getClassname(), getTitle(),
                getDescription(), getClassVarName(), getModelJson(), getDataType(), getXmlPrefix(), getXmlNamespace(),
                getXmlName(), getClassFilename(), getUnescapedDescription(), getDiscriminator(), getDefaultValue(),
                getArrayModelType(), getCircularReferences(), isAlias, isString, isInteger, isLong, isNumber, isNumeric, isFloat, isDouble,
                getVars(), getAllVars(), getRequiredVars(), getOptionalVars(), getReadOnlyVars(), getReadWriteVars(),
                getParentVars(), getAllowableValues(), getMandatory(), getAllMandatory(), getImports(), hasVars,
                isEmptyVars(), hasMoreModels, hasEnums, isEnum, isNullable, hasRequired, hasOptional, isArrayModel,
                hasChildren, isMapModel, hasOnlyReadOnly, getExternalDocumentation(), getVendorExtensions(),
                getAdditionalPropertiesType(), getMaxProperties(), getMinProperties(), getUniqueItems(), getMaxItems(),
                getMinItems(), getMaxLength(), getMinLength(), getExclusiveMinimum(), getExclusiveMaximum(), getMinimum(),
                getMaximum(), getPattern());
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
        sb.append(", circularReferences='").append(circularReferences).append('\'');
        sb.append(", isAlias=").append(isAlias);
        sb.append(", isString=").append(isString);
        sb.append(", isInteger=").append(isInteger);
        sb.append(", isLong=").append(isLong);
        sb.append(", isNumber=").append(isNumber);
        sb.append(", isNumeric=").append(isNumeric);
        sb.append(", isFloat=").append(isFloat);
        sb.append(", isDouble=").append(isDouble);
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
        sb.append(", isArrayModel=").append(isArrayModel);
        sb.append(", hasChildren=").append(hasChildren);
        sb.append(", isMapModel=").append(isMapModel);
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
        sb.append('}');
        return sb.toString();
    }

    public boolean isEmptyVars() {
        return emptyVars;
    }

    public void setEmptyVars(boolean emptyVars) {
        this.emptyVars = emptyVars;
    }

    /**
     * Remove duplicated properties in all variable list and update "hasMore"
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

        // update property list's "hasMore"
        updatePropertyListHasMore(vars);
        updatePropertyListHasMore(optionalVars);
        updatePropertyListHasMore(requiredVars);
        updatePropertyListHasMore(parentVars);
        updatePropertyListHasMore(allVars);
        updatePropertyListHasMore(readOnlyVars);
        updatePropertyListHasMore(readWriteVars);
    }

    private List<CodegenProperty> removeDuplicatedProperty(List<CodegenProperty> vars) {
        // clone the list first
        List<CodegenProperty> newList = new ArrayList<CodegenProperty>();
        for (CodegenProperty cp : vars) {
            newList.add(cp.clone());
        }

        Set<String> propertyNames = new TreeSet<String>();
        Set<String> duplicatedNames = new TreeSet<String>();

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
     * Clone the element and update "hasMore" in the list of codegen properties
     */
    private void updatePropertyListHasMore(List<CodegenProperty> vars) {
        if (vars != null) {
            for (int i = 0; i < vars.size(); i++) {
                if (i < vars.size() - 1) {
                    vars.get(i).hasMore = true;
                } else { // last element
                    vars.get(i).hasMore = false;
                }
            }
        }
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
