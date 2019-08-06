package org.openapitools.codegen.languages;

import io.swagger.util.Json;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;

import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class GoGinDriverServerCodegen extends AbstractGoCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(GoGinDriverServerCodegen.class);
    public static final String PROJECT_NAME = "openapi-server";

    protected String apiVersion = "1.0.0";
    protected int serverPort = 8080;
    protected String projectName = "openapi-server";
    protected String apiPath = "driver";

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "go-gin-driver";
    }

    public String getHelp() {
        return "Generates a go-gin-driver server.";
    }

    public GoGinDriverServerCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "go";
        modelTemplateFiles.put("model.mustache", ".go");
        apiTemplateFiles.put("controller-api.mustache", ".go");
        embeddedTemplateDir = templateDir = "go-gin-driver-server";

        // apiPackage = File.separator + "Apis";
        // modelPackage = File.separator + "Models";
        
        // supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        // TODO: Fill this out.
    }

    @Override
    public void processOpts() {
        super.processOpts();
        
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("oas3_drv");
        }

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties.put("serverPort", serverPort);
        additionalProperties.put("apiPath", apiPath);
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);

        modelPackage = apiPath;
        apiPackage = apiPath;

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("openapi.mustache", "api", "openapi.yaml"));
        supportingFiles.add(new SupportingFile("main.mustache", "", "main.go"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));
        supportingFiles.add(new SupportingFile("routers.mustache", apiPath, "routers.go"));
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
    }


    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);

        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> model : models) {
                CodegenModel cm = (CodegenModel) model.get("model");

                // merge the type properties into model
                if (!cm.isAlias && cm.isEmptyVars() && cm.getImports().size() == 1 && cm.getInterfaces().size() == 1) {
                    CodegenModel im = ModelUtils.getModelByName(toModelName(cm.getInterfaces().get(0)), result);
                    if (im != null) {
                        cm.vendorExtensions.put("inheritInterface", im);
                        LOGGER.info(String.format("append inheritInterface [%s] to [%s] model", im.name, cm.name));
                        LOGGER.debug(Json.pretty(cm));
//                        cm.interfaces.clear();
//                        cm.imports.clear();
//                        cm.allOf.clear();
//
//                        cm.dataType = im.dataType;
//                        cm.description = (cm.getDescription() == null || cm.getDescription().isEmpty()) ? im.getDescription() : cm.getDescription();
//                        cm.isString = im.isString;
//                        cm.isInteger = im.isInteger;
//                        cm.isLong = im.isLong;
//                        cm.isNumber = im.isNumber;
//                        cm.isNumeric = im.isNumeric;
//                        cm.isFloat = im.isFloat;
//                        cm.isDouble = im.isDouble;
//                        cm.isEnum = im.isEnum;
//                        cm.isNullable = im.isNullable;
//
//                        LOGGER.info(String.format("merge [%s] type properties to [%s] model", im.name, cm.name));
//                        LOGGER.debug(Json.pretty(cm));
                    }
                }
            }
        }

        return result;
    }
}