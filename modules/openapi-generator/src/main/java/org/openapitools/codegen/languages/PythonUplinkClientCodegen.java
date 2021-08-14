package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;

import java.io.File;
import java.util.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PythonUplinkClientCodegen extends AbstractPythonCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    static final Logger LOGGER = LoggerFactory.getLogger(PythonUplinkClientCodegen.class);

    private static final String DEFAULT_PACKAGE_NAME = "openapi_client";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "python-uplink";
    }

    public String getHelp() {
        return "Generates a python-uplink client.";
    }

    public PythonUplinkClientCodegen() {
        super();

        // clear import mapping (from default generator) as python does not use it
        // at the moment
        importMapping.clear();

        // TODO: remove?
        // super.processOpts();

        supportsInheritance = true;
        modelPackage = packageName + "." + "model";
        apiPackage = packageName + "." + "api";
        outputFolder = "generated-code" + File.separatorChar + "python-uplink";

        // add the models and apis folders
        supportingFiles.add(new SupportingFile("__init__model.mustache", packagePath() + File.separatorChar + "model",
                "__init__.py"));
        supportingFiles.add(
                new SupportingFile("__init__api.mustache", packagePath() + File.separatorChar + "api", "__init__.py"));

        modelTemplateFiles.put("model.mustache", ".py");
        apiTemplateFiles.put("api.mustache", ".py");

        embeddedTemplateDir = templateDir = "python-uplink";

        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "python package name (convention: snake_case).")
                .defaultValue(DEFAULT_PACKAGE_NAME));
        cliOptions
                .add(new CliOption(CodegenConstants.PACKAGE_VERSION, "python package version.").defaultValue("1.0.0"));

    }

    /***
     * Override with special post-processing for all models. we have a custom
     * version of this method to: - fix the model imports, go from model name to the
     * full import string with toModelImport + globalImportFixer
     *
     * @param objects a map going from the model name to a object holding the model
     *                info
     * @return the updated objects
     */
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objects) {
        super.postProcessAllModels(objects);

        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        for (String schemaName : allDefinitions.keySet()) {
            String modelName = toModelName(schemaName);

            HashMap<String, Object> objModel = (HashMap<String, Object>) objects.get(modelName);
            if (objModel != null) { // to avoid form parameter's models that are not generated (skipFormModel=true)
                List<Map<String, Object>> models = (List<Map<String, Object>>) objModel.get("models");
                for (Map<String, Object> model : models) {
                    CodegenModel cm = (CodegenModel) model.get("model");
                    String[] importModelNames = cm.imports.toArray(new String[0]);
                    cm.imports.clear();
                    for (String importModelName : importModelNames) {
                        cm.imports.add(toModelImport(importModelName));
                    }
                }
            }
        }

        return objects;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> models) {
        @SuppressWarnings("unchecked")
        Map<String, Object> operationsMap = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operationsMap.get("operation");
        for (CodegenOperation operation : operationList) {
            if (operation.hasProduces) {
                for (Map<String, String> produces : operation.produces) {
                    if (produces.containsKey("mediaType") && produces.get("mediaType").equals("application/json")) {
                        operation.vendorExtensions.put("x-operation-produces", "json");
                        break;
                    }
                }
            }

            if (operation.hasConsumes) {
                for (Map<String, String> consumes : operation.consumes) {
                    if (consumes.containsKey("mediaType") && consumes.get("mediaType").equals("application/json")) {
                        operation.vendorExtensions.put("x-operation-consumes", "json");
                        break;
                    }
                }
            }
        }

        return operations;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separatorChar + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separatorChar + modelPackage().replace('.', File.separatorChar);
    }

    public String packagePath() {
        return packageName.replace('.', File.separatorChar);
    }

    @Override
    public String toModelImport(String name) {
        // name looks like Cat
        return "from " + modelPackage() + "." + toModelFilename(name) + " import " + toModelName(name);
    }
}
