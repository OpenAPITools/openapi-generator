package io.swagger.codegen.languages;

import io.swagger.codegen.*;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

public class TypeScriptAureliaClientCodegen extends AbstractTypeScriptClientCodegen {

    public static final String NPM_NAME = "npmName";
    public static final String NPM_VERSION = "npmVersion";

    protected String npmName = null;
    protected String npmVersion = "1.0.0";

    public TypeScriptAureliaClientCodegen() {
        super();

        apiTemplateFiles.put("api.mustache", ".ts");

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "generated-code/typescript-aurelia";
        embeddedTemplateDir = templateDir = "typescript-aurelia";
        this.cliOptions.add(new CliOption(NPM_NAME, "The name under which you want to publish generated npm package"));
        this.cliOptions.add(new CliOption(NPM_VERSION, "The version of your npm package"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(NPM_NAME)) {
            this.setNpmName(additionalProperties.get(NPM_NAME).toString());
        }

        if (additionalProperties.containsKey(NPM_VERSION)) {
            this.setNpmVersion(additionalProperties.get(NPM_VERSION).toString());
        }

        // Set supporting files
        supportingFiles.add(new SupportingFile("models.mustache", "", "models.ts"));
        supportingFiles.add(new SupportingFile("index.ts.mustache", "", "index.ts"));
        supportingFiles.add(new SupportingFile("Api.ts.mustache", "", "Api.ts"));
        supportingFiles.add(new SupportingFile("AuthStorage.ts.mustache", "", "AuthStorage.ts"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
        supportingFiles.add(new SupportingFile("package.json.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("tsconfig.json.mustache", "", "tsconfig.json"));
        supportingFiles.add(new SupportingFile("tslint.json.mustache", "", "tslint.json"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
    }

    @Override
    public String getName() {
        return "typescript-aurelia";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library for the Aurelia framework (beta).";
    }

    public String getNpmName() {
        return npmName;
    }

    public void setNpmName(String npmName) {
        this.npmName = npmName;
    }

    public String getNpmVersion() {
        return npmVersion;
    }

    public void setNpmVersion(String npmVersion) {
        this.npmVersion = npmVersion;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        objs = super.postProcessOperations(objs);

        HashSet<String> modelImports = new HashSet<>();
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            // Aurelia uses "asGet", "asPost", ... methods; change the method format
            op.httpMethod = initialCaps(op.httpMethod.toLowerCase());

            // Collect models to be imported
            for (CodegenParameter param : op.allParams) {
                if (!param.isPrimitiveType && !param.isListContainer && !param.dataType.equals("any")) {
                    modelImports.add(param.dataType);
                }
            }
            if (op.returnBaseType != null && !op.returnTypeIsPrimitive) {
                modelImports.add(op.returnBaseType);
            }
        }

        objs.put("modelImports", modelImports);

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
            for (CodegenProperty var : cm.vars) {
                // name enum with model name, e.g. StatuEnum => PetStatusEnum
                if (Boolean.TRUE.equals(var.isEnum)) {
                    var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
                    var.enumName = cm.classname + var.enumName;
                }
            }
        }

        return objs;
    }

}
