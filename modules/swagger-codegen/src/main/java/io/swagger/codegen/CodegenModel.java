package io.swagger.codegen;

import io.swagger.models.ExternalDocs;

import java.util.*;

public class CodegenModel {
    public String parent, parentSchema;
    public List<String> interfaces;

    // References to parent and interface CodegenModels. Only set when code generator supports inheritance.
    public CodegenModel parentModel;
    public List<CodegenModel> interfaceModels;

    public String name, classname, description, classVarName, modelJson, dataType;
    public String classFilename; // store the class file name, mainly used for import
    public String unescapedDescription;
    public String discriminator;
    public String defaultValue;
    public List<CodegenProperty> vars = new ArrayList<CodegenProperty>();
    public List<CodegenProperty> allVars;
    public List<String> allowableValues;

    // Sorted sets of required parameters.
    public Set<String> mandatory = new TreeSet<String>();
    public Set<String> allMandatory;

    public Set<String> imports = new TreeSet<String>();
    public Boolean hasVars, emptyVars, hasMoreModels, hasEnums, isEnum;
    public ExternalDocs externalDocs;

    public Map<String, Object> vendorExtensions;

    {
        // By default these are the same collections. Where the code generator supports inheritance, composed models
        // store the complete closure of owned and inherited properties in allVars and allMandatory.
        allVars = vars;
        allMandatory = mandatory;
    }
}
