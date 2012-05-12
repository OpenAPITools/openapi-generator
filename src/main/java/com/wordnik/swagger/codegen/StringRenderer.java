package com.wordnik.swagger.codegen;

import org.antlr.stringtemplate.AttributeRenderer;

class StringRenderer implements AttributeRenderer {

  @Override
  public String toString(Object o) {
    if (o == null) {
      return "";
    }
    
    return o.toString();
  }

  @Override
  public String toString(Object o, String formatName) {
    if (o == null) {
      return "";
    }
    
    String result = o.toString();
    
    if (formatName != null && "xml-safe".equals(formatName)) {
      result = result.replace("<", "%lt;").replace(">", "&gt;");
    }
    
    return result;
  }
  
}
