package io.swagger.codegen.languages;

import io.swagger.codegen.SupportingFile;

import java.io.File;

public class TypeScriptFetchClientCodegen extends AbstractTypeScriptClientCodegen {

    @Override
    public String getName() {
        return "typescript-fetch";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library using Fetch API (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        final String defaultFolder = apiPackage().replace('.', File.separatorChar);

        supportingFiles.add(new SupportingFile("api.mustache", null, "api.ts"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("assign.ts", defaultFolder, "assign.ts"));
        supportingFiles.add(new SupportingFile("package.json", "", "package.json"));
        supportingFiles.add(new SupportingFile("typings.json", "", "typings.json"));
        supportingFiles.add(new SupportingFile("tsconfig.json", "", "tsconfig.json"));
    }

    public TypeScriptFetchClientCodegen() {
        super();
        outputFolder = "generated-code/typescript-fetch";
        embeddedTemplateDir = templateDir = "TypeScript-Fetch";
    }

}
