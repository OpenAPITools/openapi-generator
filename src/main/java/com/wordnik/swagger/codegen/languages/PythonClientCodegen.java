/**
 *  Copyright 2014 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;

public class PythonClientCodegen extends DefaultCodegen implements CodegenConfig {

	  public String getName() {
	    return "python";
	  }

	  public String getHelp() {
	    return "Generates a Python client library.";
	  }

	  public PythonClientCodegen() {
	    super();
	    outputFolder = "generated-code/python";
	    modelTemplateFiles.put("model.mustache", ".py");
	    apiTemplateFiles.put("api.mustache", ".py");
	    templateDir = "python";
	    
	    apiPackage = "";
	    modelPackage = "models";
	    
	    languageSpecificPrimitives.clear();
	    languageSpecificPrimitives.add("int");
	    languageSpecificPrimitives.add("float");
	    languageSpecificPrimitives.add("long");
	    languageSpecificPrimitives.add("list");
	    languageSpecificPrimitives.add("bool");
	    languageSpecificPrimitives.add("str");
	    languageSpecificPrimitives.add("datetime");

	    typeMapping.clear();
	    typeMapping.put("integer", "int");
	    typeMapping.put("float", "float");
	    typeMapping.put("long", "long");
	    typeMapping.put("double", "float");
	    typeMapping.put("array", "list");
	    typeMapping.put("map", "map");
	    typeMapping.put("boolean", "bool");
	    typeMapping.put("string", "str");
	    typeMapping.put("date", "datetime");

	    
	    supportingFiles.add(new SupportingFile("swagger.mustache", "", "swagger.py"));
	    supportingFiles.add(new SupportingFile("__init__.mustache", "", "__init__.py"));
	    supportingFiles.add(new SupportingFile("__init__.mustache", modelPackage, "__init__.py"));
	    
	  }

	  @Override
	  public String escapeReservedWord(String name) {
	    return "_" + name;
	  }

	  @Override
	  public String apiFileFolder() {
	    return outputFolder + "/" + apiPackage().replaceAll("\\.", "/");
	  }

	  public String modelFileFolder() {
	    return outputFolder + "/" + modelPackage().replaceAll("\\.", "/");
	  }

	  @Override
	  public String getTypeDeclaration(Property p) {
	    if(p instanceof ArrayProperty) {
	      ArrayProperty ap = (ArrayProperty) p;
	      Property inner = ap.getItems();
	      return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
	    }
	    else if (p instanceof MapProperty) {
	      MapProperty mp = (MapProperty) p;
	      Property inner = mp.getAdditionalProperties();

	      return getSwaggerType(p) + "(String, " + getTypeDeclaration(inner) + ")";
	    }
	    return super.getTypeDeclaration(p);
	  }

	  @Override
	  public String getSwaggerType(Property p) {
	    String swaggerType = super.getSwaggerType(p);
	    String type = null;
	    if(typeMapping.containsKey(swaggerType)) {
	      type = typeMapping.get(swaggerType);
	      if(languageSpecificPrimitives.contains(type)) {
	        return type;
	      }
	    }
	    else
	      type = swaggerType;
	    return type;
	  }

	  public String toDefaultValue(Property p) {
		// TODO: Support Python def value
	    return "null";
	  }	
}
