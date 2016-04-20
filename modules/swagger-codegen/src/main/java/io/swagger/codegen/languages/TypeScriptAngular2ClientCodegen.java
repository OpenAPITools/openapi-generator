package io.swagger.codegen.languages;

import java.io.File;

import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.FileProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

public class TypeScriptAngular2ClientCodegen extends AbstractTypeScriptClientCodegen {

    public TypeScriptAngular2ClientCodegen() {
        super();
        this.outputFolder = "generated-code/typescript-angular2";

        embeddedTemplateDir = templateDir = "typescript-angular2";
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");
        typeMapping.put("Date","Date");
        apiPackage = "api";
        modelPackage = "model";
    }

    @Override
    public String getName() {
        return "typescript-angular2";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript Angular2 client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.clear();

        supportingFiles.add(new SupportingFile("models.mustache", modelPackage().replace('.', File.separatorChar), "models.ts"));
        supportingFiles.add(new SupportingFile("apis.mustache", apiPackage().replace('.', File.separatorChar), "api.ts"));
        supportingFiles.add(new SupportingFile("index.mustache", getIndexDirectory(), "index.ts"));
    }

    private String getIndexDirectory() {
        String indexPackage = modelPackage.substring(0, Math.max(0, modelPackage.lastIndexOf('.')));
        return indexPackage.replace('.', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        Property inner;
        if(p instanceof ArrayProperty) {
            ArrayProperty mp1 = (ArrayProperty)p;
            inner = mp1.getItems();
            return this.getSwaggerType(p) + "<" + this.getTypeDeclaration(inner) + ">";
        } else if(p instanceof MapProperty) {
            MapProperty mp = (MapProperty)p;
            inner = mp.getAdditionalProperties();
            return "{ [key: string]: " + this.getTypeDeclaration(inner) + "; }";
        } else {
            return p instanceof FileProperty ? "any" : super.getTypeDeclaration(p);
        }
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        return addModelPrefix(swaggerType);
    }

    private String addModelPrefix(String swaggerType) {
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type))
                return type;
        } else
            type = "models." + swaggerType;
        return type;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        parameter.dataType = addModelPrefix(parameter.dataType);
    }

}
