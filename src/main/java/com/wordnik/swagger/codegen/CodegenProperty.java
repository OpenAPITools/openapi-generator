package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;

public class CodegenProperty {
  public String getter, setter, description, datatype, name, min, max, defaultValue;
  public Map<String, Object> allowableValues;
}