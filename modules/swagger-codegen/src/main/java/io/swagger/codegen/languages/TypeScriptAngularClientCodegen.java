package io.swagger.codegen.languages;

import io.swagger.codegen.SupportingFile;
import java.io.File;

public class TypeScriptAngularClientCodegen extends AbstractTypeScriptClientCodegen {

	@Override
	public String getName() {
		return "typescript-angular";
	}

	@Override
	public String getHelp() {
		return "Generates a TypeScript AngularJS client library.";
	}
	
	@Override
    public void processOpts() {
        super.processOpts();
	    supportingFiles.add(new SupportingFile("api.d.mustache", apiPackage().replace('.', File.separatorChar), "api.d.ts"));
	}
	
	public TypeScriptAngularClientCodegen() {
	    super();
	    outputFolder = "generated-code/typescript-angular";
	    modelTemplateFiles.put("model.mustache", ".ts");
	    apiTemplateFiles.put("api.mustache", ".ts");
	    embeddedTemplateDir = templateDir = "TypeScript-Angular";
	    apiPackage = "API.Client";
	    modelPackage = "API.Client";
	}
}
