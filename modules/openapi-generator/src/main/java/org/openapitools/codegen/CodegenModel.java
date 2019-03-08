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
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.*;

@JsonIgnoreProperties({"parentModel", "interfaceModels"})
public class CodegenModel {
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
    public boolean isAlias; // Is this effectively an alias of another simple type
    public boolean isString, isInteger;
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
    public boolean hasVars, emptyVars, hasMoreModels, hasEnums, isEnum, hasRequired, hasOptional, isArrayModel, hasChildren, isMapModel;
    public boolean hasOnlyReadOnly = true; // true if all properties are read-only
    public ExternalDocumentation externalDocumentation;

    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();

    //The type of the value from additional properties. Used in map like objects.
    public String additionalPropertiesType;

    @Override
    public String toString() {
        return new ToStringBuilder(this)
                .append("parent", parent)
                .append("parentSchema", parentSchema)
                .append("interfaces", interfaces)
                .append("parentModel", parentModel)
                .append("interfaceModels", interfaceModels)
                .append("children", children)
                .append("name", name)
                .append("classname", classname)
                .append("title", title)
                .append("description", description)
                .append("classVarName", classVarName)
                .append("modelJson", modelJson)
                .append("dataType", dataType)
                .append("xmlPrefix", xmlPrefix)
                .append("xmlNamespace", xmlNamespace)
                .append("xmlName", xmlName)
                .append("classFilename", classFilename)
                .append("unescapedDescription", unescapedDescription)
                .append("discriminator", discriminator)
                .append("defaultValue", defaultValue)
                .append("arrayModelType", arrayModelType)
                .append("isAlias", isAlias)
                .append("isString", isString)
                .append("isInteger", isInteger)
                .append("vars", vars)
                .append("requiredVars", requiredVars)
                .append("optionalVars", optionalVars)
                .append("readOnlyVars", readOnlyVars)
                .append("readWriteVars", readWriteVars)
                .append("allVars", allVars)
                .append("parentVars", parentVars)
                .append("allowableValues", allowableValues)
                .append("mandatory", mandatory)
                .append("allMandatory", allMandatory)
                .append("imports", imports)
                .append("hasVars", hasVars)
                .append("emptyVars", emptyVars)
                .append("hasMoreModels", hasMoreModels)
                .append("hasEnums", hasEnums)
                .append("isEnum", isEnum)
                .append("hasRequired", hasRequired)
                .append("hasOptional", hasOptional)
                .append("isArrayModel", isArrayModel)
                .append("hasChildren", hasChildren)
                .append("isMapModel", isMapModel)
                .append("hasOnlyReadOnly", hasOnlyReadOnly)
                .append("externalDocumentation", externalDocumentation)
                .append("vendorExtensions", vendorExtensions)
                .append("additionalPropertiesType", additionalPropertiesType)
                .toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenModel that = (CodegenModel) o;

        return Objects.equals(parent, that.parent) &&
            Objects.equals(parentSchema, that.parentSchema) &&
            Objects.equals(interfaces, that.interfaces) &&
            Objects.equals(allParents, that.allParents) &&
            Objects.equals(parentModel, that.parentModel) &&
            Objects.equals(interfaceModels, that.interfaceModels) &&
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
            Objects.equals(vars, that.vars) &&
            Objects.equals(requiredVars, that.requiredVars) &&
            Objects.equals(optionalVars, that.optionalVars) &&
            Objects.equals(allVars, that.allVars) &&
            Objects.equals(allowableValues, that.allowableValues) &&
            Objects.equals(mandatory, that.mandatory) &&
            Objects.equals(allMandatory, that.allMandatory) &&
            Objects.equals(imports, that.imports) &&
            Objects.equals(hasVars, that.hasVars) &&
            Objects.equals(emptyVars, that.emptyVars) &&
            Objects.equals(hasMoreModels, that.hasMoreModels) &&
            Objects.equals(hasEnums, that.hasEnums) &&
            Objects.equals(isEnum, that.isEnum) &&
            Objects.equals(externalDocumentation, that.externalDocumentation) &&
            Objects.equals(hasOnlyReadOnly, that.hasOnlyReadOnly) &&
            Objects.equals(hasChildren, that.hasChildren) &&
            Objects.equals(parentVars, that.parentVars) &&
            Objects.equals(vendorExtensions, that.vendorExtensions);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
            parent,
            parentSchema,
            interfaces,
            allParents,
            parentModel,
            interfaceModels,
            name,
            classname,
            title,
            description,
            classVarName,
            modelJson,
            dataType,
            xmlPrefix,
            xmlNamespace,
            xmlName,
            classFilename,
            unescapedDescription,
            discriminator,
            defaultValue,
            vars,
            requiredVars,
            optionalVars,
            allVars,
            allowableValues,
            mandatory,
            allMandatory,
            imports,
            hasVars,
            emptyVars,
            hasMoreModels,
            hasEnums,
            isEnum,
            externalDocumentation,
            vendorExtensions,
            hasOnlyReadOnly,
            hasChildren,
            parentVars);
    }

    public String getParent() {
        return parent;
    }

    public void setParent(String parent) {
        this.parent = parent;
    }

    public String getParentSchema() {
        return parentSchema;
    }

    public void setParentSchema(String parentSchema) {
        this.parentSchema = parentSchema;
    }

    public List<String> getInterfaces() {
        return interfaces;
    }

    public List<String> getAllParents() {
        return allParents;
    }

    public void setInterfaces(List<String> interfaces) {
        this.interfaces = interfaces;
    }

    public void setAllParents(List<String> allParents) {
        this.allParents = allParents;
    }

    public CodegenModel getParentModel() {
        return parentModel;
    }

    public void setParentModel(CodegenModel parentModel) {
        this.parentModel = parentModel;
    }

    public List<CodegenModel> getInterfaceModels() {
        return interfaceModels;
    }

    public void setInterfaceModels(List<CodegenModel> interfaceModels) {
        this.interfaceModels = interfaceModels;
    }

    public List<CodegenModel> getChildren() {
        return children;
    }

    public void setChildren(List<CodegenModel> children) {
        this.children = children;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getClassname() {
        return classname;
    }

    public void setClassname(String classname) {
        this.classname = classname;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getClassVarName() {
        return classVarName;
    }

    public void setClassVarName(String classVarName) {
        this.classVarName = classVarName;
    }

    public String getModelJson() {
        return modelJson;
    }

    public void setModelJson(String modelJson) {
        this.modelJson = modelJson;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public String getXmlPrefix() {
        return xmlPrefix;
    }

    public void setXmlPrefix(String xmlPrefix) {
        this.xmlPrefix = xmlPrefix;
    }

    public String getXmlNamespace() {
        return xmlNamespace;
    }

    public void setXmlNamespace(String xmlNamespace) {
        this.xmlNamespace = xmlNamespace;
    }

    public String getXmlName() {
        return xmlName;
    }

    public void setXmlName(String xmlName) {
        this.xmlName = xmlName;
    }

    public String getClassFilename() {
        return classFilename;
    }

    public void setClassFilename(String classFilename) {
        this.classFilename = classFilename;
    }

    public String getUnescapedDescription() {
        return unescapedDescription;
    }

    public void setUnescapedDescription(String unescapedDescription) {
        this.unescapedDescription = unescapedDescription;
    }

    public CodegenDiscriminator getDiscriminator() {
        return discriminator;
    }

    public String getDiscriminatorName() {
        return discriminator == null ? null : discriminator.getPropertyName();
    }

    public void setDiscriminator(CodegenDiscriminator discriminator) {
        this.discriminator = discriminator;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public String getArrayModelType() {
        return arrayModelType;
    }

    public void setArrayModelType(String arrayModelType) {
        this.arrayModelType = arrayModelType;
    }

    public List<CodegenProperty> getVars() {
        return vars;
    }

    public void setVars(List<CodegenProperty> vars) {
        this.vars = vars;
    }

    public List<CodegenProperty> getRequiredVars() {
        return requiredVars;
    }

    public void setRequiredVars(List<CodegenProperty> requiredVars) {
        this.requiredVars = requiredVars;
    }

    public List<CodegenProperty> getOptionalVars() {
        return optionalVars;
    }

    public void setOptionalVars(List<CodegenProperty> optionalVars) {
        this.optionalVars = optionalVars;
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

    public List<CodegenProperty> getAllVars() {
        return allVars;
    }

    public void setAllVars(List<CodegenProperty> allVars) {
        this.allVars = allVars;
    }

    public List<CodegenProperty> getParentVars() {
        return parentVars;
    }

    public void setParentVars(List<CodegenProperty> parentVars) {
        this.parentVars = parentVars;
    }

    public Map<String, Object> getAllowableValues() {
        return allowableValues;
    }

    public void setAllowableValues(Map<String, Object> allowableValues) {
        this.allowableValues = allowableValues;
    }

    public Set<String> getMandatory() {
        return mandatory;
    }

    public void setMandatory(Set<String> mandatory) {
        this.mandatory = mandatory;
    }

    public Set<String> getAllMandatory() {
        return allMandatory;
    }

    public void setAllMandatory(Set<String> allMandatory) {
        this.allMandatory = allMandatory;
    }

    public Set<String> getImports() {
        return imports;
    }

    public void setImports(Set<String> imports) {
        this.imports = imports;
    }

    public boolean isEmptyVars() {
        return emptyVars;
    }

    public void setEmptyVars(boolean emptyVars) {
        this.emptyVars = emptyVars;
    }

    public ExternalDocumentation getExternalDocumentation() {
        return externalDocumentation;
    }

    public void setExternalDocumentation(ExternalDocumentation externalDocumentation) {
        this.externalDocumentation = externalDocumentation;
    }

    public Map<String, Object> getVendorExtensions() {
        return vendorExtensions;
    }

    public void setVendorExtensions(Map<String, Object> vendorExtensions) {
        this.vendorExtensions = vendorExtensions;
    }

    public String getAdditionalPropertiesType() {
        return additionalPropertiesType;
    }

    public void setAdditionalPropertiesType(String additionalPropertiesType) {
        this.additionalPropertiesType = additionalPropertiesType;
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
