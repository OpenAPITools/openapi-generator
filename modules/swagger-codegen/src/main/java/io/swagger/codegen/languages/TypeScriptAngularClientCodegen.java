package io.swagger.codegen.languages;

import java.io.File;

import io.swagger.codegen.SupportingFile;

public class TypeScriptAngularClientCodegen extends TypeScriptNodeClientCodegen {
	
	@Override
	public String getName() {
		return "typescript-angular";
	}
	
	public TypeScriptAngularClientCodegen() {
	    super();
	    outputFolder = "generated-code/typescript-angular";
	    modelTemplateFiles.put("model.mustache", ".ts");
	    apiTemplateFiles.put("api.mustache", ".ts");
	    templateDir = "TypeScript-Angular";
	    apiPackage = "api";
	    modelPackage = "api";
	    
	    supportingFiles.add(new SupportingFile("api.d.mustache", apiPackage + File.separator, "api.d.ts"));
	}
}