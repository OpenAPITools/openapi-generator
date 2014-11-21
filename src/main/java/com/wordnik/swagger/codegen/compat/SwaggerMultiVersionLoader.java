package com.wordnik.swagger.codegen.compat;

import com.wordnik.swagger.models.Swagger;
import com.wordnik.swagger.util.*;

public class SwaggerMultiVersionLoader {
  public Swagger read(String input) {
    // try 2.0 first, call legacy if null
    Swagger output = new SwaggerLoader().read(input);
    if(output == null)
      output = new SwaggerLegacyLoader().read(input);
    return output;
  }
}
