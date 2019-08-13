package org.openapitools.codegen.languages;

import com.samskivert.mustache.Mustache;
import io.swagger.util.Json;
import io.swagger.v3.oas.models.media.Schema;
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
            List<Map<String, String>> imports = (List<Map<String, String>>) inner.get("imports");

            for (Map<String, Object> model : models) {
                CodegenModel cm = (CodegenModel) model.get("model");

                if (cm.isEnum) {
                    imports.add(createMapping("import", "fmt"));
                }

                if (cm.hasVars) {
                    setInheritProperties(cm.getVars());
                    setInheritProperties(cm.getAllVars());
                    setInheritProperties(cm.getRequiredVars());
                    setInheritProperties(cm.getOptionalVars());
                    setInheritProperties(cm.getReadOnlyVars());
                    setInheritProperties(cm.getReadWriteVars());
                    setInheritProperties(cm.getParentVars());
                }

                // merge the type properties into model
                if (!cm.isAlias && cm.isEmptyVars() && cm.getImports().size() == 1 && cm.getInterfaces().size() == 1) {
                    CodegenModel im = ModelUtils.getModelByName(toModelName(cm.getInterfaces().get(0)), result);
                    cm.vendorExtensions.put("inheritInterface", im);
                    LOGGER.info(String.format("append vendorExtensions.inheritInterface [%s] to [%s] model", im.name, cm.name));
                    LOGGER.debug(Json.pretty(cm));
                    LOGGER.debug(Json.pretty(im));
                }
            }
        }

        return result;
    }

    private void setInheritProperties(List<CodegenProperty> properties) {
        Map<String, Schema> schemas = ModelUtils.getSchemas(this.openAPI);

        for(CodegenProperty prop : properties) {
            if (prop.isPrimitiveType || prop.complexType.isEmpty() || prop.isEnum) {
                continue;
            }

            Schema schema = schemas.get(prop.complexType);
            CodegenModel cm = fromModel(prop.complexType, schema);

            CodegenModel im = getNestedModel(cm);
            if (im.isEnum) {
                prop.openApiType = im.classname;
                prop.complexType = im.classname;
                prop.datatypeWithEnum = im.classname;
                prop.baseType = im.classname;
                prop.dataType = im.classname;

                continue;
            }

            Schema s = schemas.get(im.name);
            prop.isString = ModelUtils.isStringSchema(s);
            prop.isNumeric = ModelUtils.isNumberSchema(s);
            prop.isInteger = ModelUtils.isIntegerSchema(s);
            prop.isLong = ModelUtils.isLongSchema(s);
            prop.isNumber = ModelUtils.isNumberSchema(s);
            prop.isFloat = ModelUtils.isFloatSchema(s);
            prop.isDouble = ModelUtils.isDoubleSchema(s);
            prop.isByteArray = ModelUtils.isByteArraySchema(s);
            prop.isBinary = ModelUtils.isBinarySchema(s);
            prop.isBoolean = ModelUtils.isBooleanSchema(s);
            prop.isDate = ModelUtils.isDateSchema(s);
            prop.isDateTime = ModelUtils.isDateTimeSchema(s);
            prop.isUuid = ModelUtils.isUUIDSchema(s);
            prop.isUri = ModelUtils.isURISchema(s);
            prop.isEmail = ModelUtils.isEmailSchema(s);
            prop.isFreeFormObject = ModelUtils.isFreeFormObject(s);
            prop.dataType = im.dataType;
        }
    }

    private void setExportParameterType(List<CodegenParameter> codegenParameters) {
        Map<String, Schema> schemas = ModelUtils.getSchemas(this.openAPI);

        for (CodegenParameter param : codegenParameters) {
            if (param.isPrimitiveType) {
                continue;
            }

            String modelName = (String) param.vendorExtensions.get("x-exportParamName");
            if (modelName == null) {
                modelName = param.dataType;
            }

            Schema schema = schemas.get(modelName);
            if (schema != null) {
                CodegenModel cm = fromModel(modelName, schema);
                if (cm != null) {
                    CodegenModel im = getNestedModel(cm);
                    Schema s = schemas.get(im.name);
                    LOGGER.info(String.format("Set export parameter [%s] from model [%s]", param.paramName, im.name));

                    param.dataType = (ModelUtils.isArraySchema(s) || im.isEnum) ? im.classname : im.dataType;
                    param.isNullable = ModelUtils.isNullable(s);
                    param.isString = ModelUtils.isStringSchema(s);
                    param.isNumeric = ModelUtils.isNumberSchema(s);
                    param.isInteger = ModelUtils.isIntegerSchema(s);
                    param.isLong = ModelUtils.isLongSchema(s);
                    param.isNumber = ModelUtils.isNumberSchema(s);
                    param.isFloat = ModelUtils.isFloatSchema(s);
                    param.isDouble = ModelUtils.isDoubleSchema(s);
                    param.isByteArray = ModelUtils.isByteArraySchema(s);
                    param.isBinary = ModelUtils.isBinarySchema(s);
                    param.isBoolean = ModelUtils.isBooleanSchema(s);
                    param.isDate = ModelUtils.isDateSchema(s);
                    param.isDateTime = ModelUtils.isDateTimeSchema(s);
                    param.isUuid = ModelUtils.isUUIDSchema(s);
                    param.isUri = ModelUtils.isURISchema(s);
                    param.isEmail = ModelUtils.isEmailSchema(s);
                    param.isFreeFormObject = ModelUtils.isFreeFormObject(s);
                }
            }
        }
    }

    private CodegenModel getNestedModel (CodegenModel cm) {
        Map<String, Schema> schemas = ModelUtils.getSchemas(this.openAPI);
        Schema schema = schemas.get(cm.name);

        if (schema != null) {
            if (cm.getInterfaces() != null && cm.getInterfaces().size() == 1) {
                CodegenModel im = fromModel(cm.getInterfaces().get(0), schema);
                return getNestedModel(im);
            }
        } else {
            LOGGER.warn("Schema {} not found", cm.name);
        }

        return cm;
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

            setExportParameterType(op.queryParams);
            setExportParameterType(op.formParams);
            setExportParameterType(op.headerParams);
            setExportParameterType(op.bodyParams);
            setExportParameterType(op.cookieParams);
            setExportParameterType(op.optionalParams);
            setExportParameterType(op.requiredParams);
            setExportParameterType(op.pathParams);
        }
        return objs;
    }
}