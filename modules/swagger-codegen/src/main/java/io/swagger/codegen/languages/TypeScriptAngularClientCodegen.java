package io.swagger.codegen.languages;

import io.swagger.codegen.SupportingFile;

public class TypeScriptAngularClientCodegen extends AbstractTypeScriptClientCodegen {
	
	@Override
	public String getName() {
		return "typescript-angular";
	}

	public String getHelp() {
		return "Generates a TypeScript AngurlarJS client library.";
	}

	public TypeScriptAngularClientCodegen() {
	    super();
	    outputFolder = "generated-code/typescript-angular";
	    modelTemplateFiles.put("model.mustache", ".ts");
	    apiTemplateFiles.put("api.mustache", ".ts");
	    templateDir = "TypeScript-Angular";
	    apiPackage = "api";
	    modelPackage = "model";
	    supportingFiles.add(new SupportingFile("api.d.mustache", apiPackage, "api.d.ts"));
	}
}