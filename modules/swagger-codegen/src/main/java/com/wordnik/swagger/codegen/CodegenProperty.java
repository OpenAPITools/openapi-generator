package com.wordnik.swagger.codegen;

import java.util.*;

public class CodegenProperty {
  public String baseName, complexType, getter, setter, description, datatype, datatypeWithEnum,
    name, min, max, defaultValue, baseType, containerType;

  /** maxLength validation for strings, see http://json-schema.org/latest/json-schema-validation.html#rfc.section.5.2.1 */
  public Integer maxLength;
  /** minLength validation for strings, see http://json-schema.org/latest/json-schema-validation.html#rfc.section.5.2.2 */
  public Integer minLength;
  /** pattern validation for strings, see http://json-schema.org/latest/json-schema-validation.html#rfc.section.5.2.3 */
  public String pattern;
  /** A free-form property to include an example of an instance for this schema. */
  public String example;

  public String jsonSchema;
  public Double minimum, maximum, exclusiveMinimum, exclusiveMaximum;
  public Boolean hasMore = null, required = null, secondaryParam = null;
  public Boolean isPrimitiveType, isContainer, isNotContainer;
  public boolean isEnum;
  public List<String> _enum;
  public Map<String, Object> allowableValues;
}
