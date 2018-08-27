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

import io.swagger.v3.oas.models.ExternalDocumentation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties({"parentModel", "interfaceModels"})
public class CodegenModel {
    public String parent, parentSchema;
    public List<String> interfaces;

    // References to parent and interface CodegenModels. Only set when code generator supports inheritance.
    public CodegenModel parentModel;
    public List<CodegenModel> interfaceModels;
    public List<CodegenModel> children;

    public String name, classname, title, description, classVarName, modelJson, dataType, xmlPrefix, xmlNamespace, xmlName;
    public String classFilename; // store the class file name, mainly used for import
    public String unescapedDescription;
    public CodegenDiscriminator discriminator;
    public String defaultValue;
    public String arrayModelType;
    public boolean isAlias; // Is this effectively an alias of another simple type
    public boolean isString, isInteger;
    public List<CodegenProperty> vars = new ArrayList<CodegenProperty>();
    public List<CodegenProperty> requiredVars = new ArrayList<CodegenProperty>(); // a list of required properties
    public List<CodegenProperty> optionalVars = new ArrayList<CodegenProperty>(); // a list of optional properties
    public List<CodegenProperty> readOnlyVars = new ArrayList<CodegenProperty>(); // a list of read-only properties
    public List<CodegenProperty> readWriteVars = new ArrayList<CodegenProperty>(); // a list of properties for read, write
    public List<CodegenProperty> allVars;
    public List<CodegenProperty> parentVars = new ArrayList<>();
    public Map<String, Object> allowableValues;

    // Sorted sets of required parameters.
    public Set<String> mandatory = new TreeSet<String>();
    public Set<String> allMandatory;

    public Set<String> imports = new TreeSet<String>();
    public boolean hasVars, emptyVars, hasMoreModels, hasEnums, isEnum, hasRequired, hasOptional, isArrayModel, hasChildren, isMapModel;
    public boolean hasOnlyReadOnly = true; // true if all properties are read-only
    public ExternalDocumentation externalDocumentation;

    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();

    //The type of the value from additional properties. Used in map like objects.
    public String additionalPropertiesType;

    {
        // By default these are the same collections. Where the code generator supports inheritance, composed models
        // store the complete closure of owned and inherited properties in allVars and allMandatory.
        allVars = vars;
        allMandatory = mandatory;
    }

    @Override
    public String toString() {
        return String.format(Locale.ROOT, "%s(%s)", name, classname);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenModel that = (CodegenModel) o;

        if (parent != null ? !parent.equals(that.parent) : that.parent != null)
            return false;
        if (parentSchema != null ? !parentSchema.equals(that.parentSchema) : that.parentSchema != null)
            return false;
        if (interfaces != null ? !interfaces.equals(that.interfaces) : that.interfaces != null)
            return false;
        if (parentModel != null ? !parentModel.equals(that.parentModel) : that.parentModel != null)
            return false;
        if (interfaceModels != null ? !interfaceModels.equals(that.interfaceModels) : that.interfaceModels != null)
            return false;
        if (name != null ? !name.equals(that.name) : that.name != null)
            return false;
        if (classname != null ? !classname.equals(that.classname) : that.classname != null)
            return false;
        if (title != null ? !title.equals(that.title) : that.title != null)
            return false;
        if (description != null ? !description.equals(that.description) : that.description != null)
            return false;
        if (classVarName != null ? !classVarName.equals(that.classVarName) : that.classVarName != null)
            return false;
        if (modelJson != null ? !modelJson.equals(that.modelJson) : that.modelJson != null)
            return false;
        if (dataType != null ? !dataType.equals(that.dataType) : that.dataType != null)
            return false;
        if (xmlPrefix != null ? !xmlPrefix.equals(that.xmlPrefix) : that.xmlPrefix != null)
            return false;
        if (xmlNamespace != null ? !xmlNamespace.equals(that.xmlNamespace) : that.xmlNamespace != null)
            return false;
        if (xmlName != null ? !xmlName.equals(that.xmlName) : that.xmlName != null)
            return false;
        if (classFilename != null ? !classFilename.equals(that.classFilename) : that.classFilename != null)
            return false;
        if (unescapedDescription != null ? !unescapedDescription.equals(that.unescapedDescription) : that.unescapedDescription != null)
            return false;
        if (discriminator != null ? !discriminator.equals(that.discriminator) : that.discriminator != null)
            return false;
        if (defaultValue != null ? !defaultValue.equals(that.defaultValue) : that.defaultValue != null)
            return false;
        if (vars != null ? !vars.equals(that.vars) : that.vars != null)
            return false;
        if (requiredVars != null ? !requiredVars.equals(that.requiredVars) : that.requiredVars != null)
            return false;
        if (optionalVars != null ? !optionalVars.equals(that.optionalVars) : that.optionalVars != null)
            return false;
        if (allVars != null ? !allVars.equals(that.allVars) : that.allVars != null)
            return false;
        if (allowableValues != null ? !allowableValues.equals(that.allowableValues) : that.allowableValues != null)
            return false;
        if (mandatory != null ? !mandatory.equals(that.mandatory) : that.mandatory != null)
            return false;
        if (allMandatory != null ? !allMandatory.equals(that.allMandatory) : that.allMandatory != null)
            return false;
        if (imports != null ? !imports.equals(that.imports) : that.imports != null)
            return false;
        if (hasVars != that.hasVars)
            return false;
        if (emptyVars != that.emptyVars)
            return false;
        if (hasMoreModels != that.hasMoreModels)
            return false;
        if (hasEnums != that.hasEnums)
            return false;
        if (isEnum != that.isEnum)
            return false;
        if (externalDocumentation != null ? !externalDocumentation.equals(that.externalDocumentation) : that.externalDocumentation != null)
            return false;
        if (!Objects.equals(hasOnlyReadOnly, that.hasOnlyReadOnly))
            return false;
        if (!Objects.equals(hasChildren, that.hasChildren))
            return false;
        if (!Objects.equals(parentVars, that.parentVars))
            return false;
        return vendorExtensions != null ? vendorExtensions.equals(that.vendorExtensions) : that.vendorExtensions == null;

    }

    @Override
    public int hashCode() {
        int result = parent != null ? parent.hashCode() : 0;
        result = 31 * result + (parentSchema != null ? parentSchema.hashCode() : 0);
        result = 31 * result + (interfaces != null ? interfaces.hashCode() : 0);
        result = 31 * result + (parentModel != null ? parentModel.hashCode() : 0);
        result = 31 * result + (interfaceModels != null ? interfaceModels.hashCode() : 0);
        result = 31 * result + (name != null ? name.hashCode() : 0);
        result = 31 * result + (classname != null ? classname.hashCode() : 0);
        result = 31 * result + (title != null ? title.hashCode() : 0);
        result = 31 * result + (description != null ? description.hashCode() : 0);
        result = 31 * result + (classVarName != null ? classVarName.hashCode() : 0);
        result = 31 * result + (modelJson != null ? modelJson.hashCode() : 0);
        result = 31 * result + (dataType != null ? dataType.hashCode() : 0);
        result = 31 * result + (xmlPrefix != null ? xmlPrefix.hashCode() : 0);
        result = 31 * result + (xmlNamespace != null ? xmlNamespace.hashCode() : 0);
        result = 31 * result + (xmlName != null ? xmlName.hashCode() : 0);
        result = 31 * result + (classFilename != null ? classFilename.hashCode() : 0);
        result = 31 * result + (unescapedDescription != null ? unescapedDescription.hashCode() : 0);
        result = 31 * result + (discriminator != null ? discriminator.hashCode() : 0);
        result = 31 * result + (defaultValue != null ? defaultValue.hashCode() : 0);
        result = 31 * result + (vars != null ? vars.hashCode() : 0);
        result = 31 * result + (requiredVars != null ? requiredVars.hashCode() : 0);
        result = 31 * result + (optionalVars != null ? optionalVars.hashCode() : 0);
        result = 31 * result + (allVars != null ? allVars.hashCode() : 0);
        result = 31 * result + (allowableValues != null ? allowableValues.hashCode() : 0);
        result = 31 * result + (mandatory != null ? mandatory.hashCode() : 0);
        result = 31 * result + (allMandatory != null ? allMandatory.hashCode() : 0);
        result = 31 * result + (imports != null ? imports.hashCode() : 0);
        result = 31 * result + (hasVars ? 13:31);
        result = 31 * result + (emptyVars ? 13:31);
        result = 31 * result + (hasMoreModels ? 13:31);
        result = 31 * result + (hasEnums ? 13:31);
        result = 31 * result + (isEnum ? 13:31);
        result = 31 * result + (externalDocumentation != null ? externalDocumentation.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
        result = 31 * result + Objects.hash(hasOnlyReadOnly);
        result = 31 * result + Objects.hash(hasChildren);
        result = 31 * result + Objects.hash(parentVars);
        return result;
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

    public void setInterfaces(List<String> interfaces) {
        this.interfaces = interfaces;
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
}
