package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;

public class CodegenProperty {
  public String baseName, complexType, getter, setter, description, datatype, name, 
    min, max, defaultValue;
  public Double minimum, maximum, exclusiveMinimum, exclusiveMaximum;
  public Boolean hasMore = null, required = null;
  public boolean isPrimitiveType, isContainer, isNotContainer;
  public List<String> _enum;
  public Map<String, Object> allowableValues;
}
