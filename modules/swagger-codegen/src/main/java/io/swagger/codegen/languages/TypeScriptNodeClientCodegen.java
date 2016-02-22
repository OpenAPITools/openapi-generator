package io.swagger.codegen.languages;

import io.swagger.codegen.SupportingFile;

public class TypeScriptNodeClientCodegen extends AbstractTypeScriptClientCodegen {

    @Override
    public String getName() {
        return "typescript-node";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript nodejs client library.";
    }

	@Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.add(new SupportingFile("api.mustache", null, "api.ts"));
    }
    
    public TypeScriptNodeClientCodegen() {
        super();
        outputFolder = "generated-code/typescript-node";
        embeddedTemplateDir = templateDir = "TypeScript-node";
    }

}
