package io.swagger.codegen.languages;

import java.io.File;

import io.swagger.codegen.SupportingFile;

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
		supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
		//supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

	}
	
	public TypeScriptAngularClientCodegen() {
	    super();
	    outputFolder = "generated-code/typescript-angular";
	    modelTemplateFiles.put("model.mustache", ".ts");
	    apiTemplateFiles.put("api.mustache", ".ts");
	    embeddedTemplateDir = templateDir = "typescript-angular";
	    apiPackage = "API.Client";
	    modelPackage = "API.Client";
	}
}
