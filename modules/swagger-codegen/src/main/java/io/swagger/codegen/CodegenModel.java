package io.swagger.codegen;

import io.swagger.models.ExternalDocs;

import java.util.*;

public class CodegenModel {
    public String parent, parentSchema;
    public String name, classname, description, classVarName, modelJson, dataType;
    public String unescapedDescription;
    public String defaultValue;
    public List<CodegenProperty> vars = new ArrayList<CodegenProperty>();
    public List<String> allowableValues;

    // list of all required parameters
    public Set<String> mandatory = new HashSet<String>();
    
    public Set<String> imports = new TreeSet<String>();
    public Boolean hasVars, emptyVars, hasMoreModels, hasEnums, isEnum;
    public ExternalDocs externalDocs;

    public Map<String, Object> vendorExtensions;
}
