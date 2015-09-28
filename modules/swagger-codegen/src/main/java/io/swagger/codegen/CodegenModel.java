package io.swagger.codegen;

import io.swagger.models.ExternalDocs;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class CodegenModel {
    public String parent;
    public String name, classname, description, classVarName, modelJson;
    public String unescapedDescription;
    public String defaultValue;
    public List<CodegenProperty> vars = new ArrayList<CodegenProperty>();
    public Set<String> imports = new HashSet<String>();
    public Boolean hasVars, emptyVars, hasMoreModels, hasEnums;
    public ExternalDocs externalDocs;
}
