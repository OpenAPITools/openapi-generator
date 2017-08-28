package io.swagger.codegen.languages;

import java.io.File;

import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.CodegenParameter;
import io.swagger.models.properties.Property;

public class TypeScriptAngularJsClientCodegen extends AbstractTypeScriptClientCodegen {

    @Override
    public String getName() {
        return "typescript-angularjs";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript AngularJS client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.add(new SupportingFile("models.mustache", modelPackage().replace('.', File.separatorChar), "models.ts"));
        supportingFiles.add(new SupportingFile("apis.mustache", apiPackage().replace('.', File.separatorChar), "api.ts"));
        supportingFiles.add(new SupportingFile("index.mustache", getIndexDirectory(), "index.ts"));
        supportingFiles.add(new SupportingFile("api.module.mustache", getIndexDirectory(), "api.module.ts"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));

    }
    
    public TypeScriptAngularJsClientCodegen() {
        super();
        outputFolder = "generated-code/typescript-angularjs";
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");
        embeddedTemplateDir = templateDir = "typescript-angularjs";
        apiPackage = "api";
        modelPackage = "model";
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        if(isLanguagePrimitive(swaggerType) || isLanguageGenericType(swaggerType)) {
            return swaggerType;
        }
        return addModelPrefix(swaggerType);
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        parameter.dataType = addModelPrefix(parameter.dataType);
    }

    private String getIndexDirectory() {
        String indexPackage = modelPackage.substring(0, Math.max(0, modelPackage.lastIndexOf('.')));
        return indexPackage.replace('.', File.separatorChar);
    }

    private String addModelPrefix(String swaggerType) {
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
        } else {
            type = swaggerType;
        }

        if (!isLanguagePrimitive(type) && !isLanguageGenericType(type)) {
            type = "models." + swaggerType;
        }
        return type;
    }

    private boolean isLanguagePrimitive(String type) {
        return languageSpecificPrimitives.contains(type);
    }

    private boolean isLanguageGenericType(String type) {
        for (String genericType: languageGenericTypes) {
            if (type.startsWith(genericType + "<"))  {
                return true;
            }
        }
        return false;
    }
}
