package io.swagger.codegen;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Objects;

import io.swagger.models.ExternalDocs;


public class CodegenModel {
    public String parent, parentSchema;
    public List<String> interfaces;

    // References to parent and interface CodegenModels. Only set when code generator supports inheritance.
    public CodegenModel parentModel;
    public List<CodegenModel> interfaceModels;

    public String name, classname, title, description, classVarName, modelJson, dataType;
    public String classFilename; // store the class file name, mainly used for import
    public String unescapedDescription;
    public String discriminator;
    public String defaultValue;
    public String arrayModelType;
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
    public Boolean hasVars, emptyVars, hasMoreModels, hasEnums, isEnum, hasRequired, isArrayModel, hasChildren;
    public Boolean hasOnlyReadOnly = true; // true if all properties are read-only
    public ExternalDocs externalDocs;

    public Map<String, Object> vendorExtensions;

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
        return String.format("%s(%s)", name, classname);
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
        if (hasVars != null ? !hasVars.equals(that.hasVars) : that.hasVars != null)
            return false;
        if (emptyVars != null ? !emptyVars.equals(that.emptyVars) : that.emptyVars != null)
            return false;
        if (hasMoreModels != null ? !hasMoreModels.equals(that.hasMoreModels) : that.hasMoreModels != null)
            return false;
        if (hasEnums != null ? !hasEnums.equals(that.hasEnums) : that.hasEnums != null)
            return false;
        if (isEnum != null ? !isEnum.equals(that.isEnum) : that.isEnum != null)
            return false;
        if (externalDocs != null ? !externalDocs.equals(that.externalDocs) : that.externalDocs != null)
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
        result = 31 * result + (hasVars != null ? hasVars.hashCode() : 0);
        result = 31 * result + (emptyVars != null ? emptyVars.hashCode() : 0);
        result = 31 * result + (hasMoreModels != null ? hasMoreModels.hashCode() : 0);
        result = 31 * result + (hasEnums != null ? hasEnums.hashCode() : 0);
        result = 31 * result + (isEnum != null ? isEnum.hashCode() : 0);
        result = 31 * result + (externalDocs != null ? externalDocs.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
        result = 31 * result + Objects.hash(hasOnlyReadOnly);
        result = 31 * result + Objects.hash(hasChildren);
        result = 31 * result + Objects.hash(parentVars);
        return result;
    }
}
