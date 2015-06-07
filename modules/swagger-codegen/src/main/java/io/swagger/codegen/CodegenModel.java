package io.swagger.codegen;

import io.swagger.models.*;
import io.swagger.models.properties.*;

import java.util.*;

public class CodegenModel {
  public String parent;
  public String name, classname, description, classVarName, modelJson;
  public String defaultValue;
  public List<CodegenProperty> vars = new ArrayList<CodegenProperty>();
  public Set<String> imports = new HashSet<String>();
  public Boolean hasVars, emptyVars, hasMoreModels, hasEnums;
  public ExternalDocs externalDocs;
}