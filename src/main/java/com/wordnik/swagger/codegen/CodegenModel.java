package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;

class CodegenModel {
  public String name, classname, description;
  public String defaultValue;
  public List<CodegenProperty> vars = new ArrayList<CodegenProperty>();
}