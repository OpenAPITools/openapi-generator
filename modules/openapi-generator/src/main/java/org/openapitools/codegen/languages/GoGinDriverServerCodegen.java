package org.openapitools.codegen.languages;

import com.samskivert.mustache.Mustache;
import io.swagger.util.Json;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;

import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.openapitools.codegen.utils.StringUtils.camelize;

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

        additionalProperties.put("camelize",
                (Mustache.Lambda) (fragment, writer) -> writer.write(camelize(fragment.execute(), false)));

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
                        LOGGER.info(String.format("append vendorExtensions.inheritInterface [%s] to [%s] model", im.name, cm.name));
                        LOGGER.debug(Json.pretty(cm));
                    }
                }
            }
        }

        return result;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            if (op.path != null) {
                op.path = op.path.replaceAll("\\{(.*?)\\}", ":$1");
            }
        }
        return objs;
    }
}