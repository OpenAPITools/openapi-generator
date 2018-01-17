package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.DateSchema;
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.FileSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.responses.ApiResponse;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;


import java.io.IOException;
import java.io.Writer;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public class AdaCodegen extends AbstractAdaCodegen implements CodegenConfig {

    public AdaCodegen() {
        super();
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "ada";
    }

    @Override
    public String getHelp() {
        return "Generates an Ada client implementation (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            packageName = (String) additionalProperties.get(CodegenConstants.PACKAGE_NAME);
        }
        if (packageName == "") {
            packageName = modelPackage;
        }
        String srcPrefix = "src" + File.separator;
        String modelPrefix = srcPrefix + "model" + File.separator + toFilename(modelPackage);
        String clientPrefix = srcPrefix + "client" + File.separator + toFilename(modelPackage);
        supportingFiles.add(new SupportingFile("model-spec.mustache", null, modelPrefix + "-models.ads"));
        supportingFiles.add(new SupportingFile("model-body.mustache", null, modelPrefix + "-models.adb"));
        supportingFiles.add(new SupportingFile("client-spec.mustache", null, clientPrefix + "-clients.ads"));
        supportingFiles.add(new SupportingFile("client-body.mustache", null, clientPrefix + "-clients.adb"));

        if (additionalProperties.containsKey(CodegenConstants.PROJECT_NAME)) {
            projectName = (String) additionalProperties.get(CodegenConstants.PROJECT_NAME);
        } else {
            // default: set project based on package name
            // e.g. petstore.api (package name) => petstore_api (project name)
            projectName = packageName.replaceAll("\\.", "_");
        }
        String configBaseName = modelPackage.toLowerCase();
        supportingFiles.add(new SupportingFile("gnat-project.mustache", "", toFilename(projectName) + ".gpr"));
        // supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("config.gpr", "", "config.gpr"));

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("package", this.modelPackage);
        additionalProperties.put("packageConfig", configBaseName);
        additionalProperties.put("packageDir", "client");
        additionalProperties.put("mainName", "client");
        additionalProperties.put(CodegenConstants.PROJECT_NAME, projectName);
        String names[] = this.modelPackage.split("\\.");
        String pkgName = names[0];
        additionalProperties.put("packageLevel1", pkgName);
        supportingFiles.add(new SupportingFile("package-spec-level1.mustache", null,
                "src" + File.separator + toFilename(names[0]) + ".ads"));
        if (names.length > 1) {
            String fileName = toFilename(names[0]) + "-" + toFilename(names[1]) + ".ads";
            pkgName = names[0] + "." + names[1];
            additionalProperties.put("packageLevel2", pkgName);
            supportingFiles.add(new SupportingFile("package-spec-level2.mustache", null,
                    "src" + File.separator + fileName));
        }
        pkgName = this.modelPackage;
        supportingFiles.add(new SupportingFile("client.mustache", null,
                "src" + File.separator + toFilename(pkgName) + "-client.adb"));
        additionalProperties.put("packageName", toFilename(pkgName));
        // add lambda for mustache templates
        additionalProperties.put("lambdaAdaComment", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                String content = fragment.execute();
                content = content.trim().replaceAll("\n$", "");
                writer.write(content.replaceAll("\n", "\n   --  "));
            }
        });

    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/model/" + modelPackage().replace('.', File.separatorChar);
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle
     * escaping those terms here. This logic is only called if a variable
     * matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return "p_" + name; // add an underscore to the name
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    /**
     * Optional - type declaration. This is a String which is used by the
     * templates to instantiate your types. There is typically special handling
     * for different property types
     *
     * @return a string value used as the `dataType` field for model templates,
     *         `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema propertySchema) {
        String swaggerType = getSchemaType(propertySchema);

        if (propertySchema instanceof ArraySchema) {
            ArraySchema arraySchema = (ArraySchema) propertySchema;
            return String.format("%s_Vectors.Vector", getTypeDeclaration(arraySchema.getItems()));
        }
        if (propertySchema instanceof MapSchema && hasSchemaProperties(propertySchema)) {
            return String.format("Swagger._Map", getTypeDeclaration((Schema) propertySchema.getAdditionalProperties()));
        }
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }
        //  LOGGER.info("Swagger type " + swaggerType);
        if (languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }
        String modelType = toModelName(swaggerType);
        if (propertySchema instanceof StringSchema || propertySchema instanceof DateSchema
                || propertySchema instanceof DateTimeSchema|| propertySchema instanceof FileSchema
                || languageSpecificPrimitives.contains(modelType)) {
            return modelType;
        }

        return modelPackage + ".Models." + modelType;
    }

    /**
     * Overrides postProcessParameter to add a vendor extension "x-is-model-type".
     * This boolean indicates that the parameter comes from the model package.
     *
     * @param parameter CodegenParameter object to be processed.
     */
    @Override
    public void postProcessParameter(CodegenParameter parameter){
        // Give the base class a chance to process
        super.postProcessParameter(parameter);

        boolean isModel = parameter.dataType.startsWith(modelPackage);
        boolean isPrimitiveType = getBooleanValue(parameter, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME);
        boolean isDate = getBooleanValue(parameter, CodegenConstants.IS_DATE_TIME_EXT_NAME);
        boolean isString = getBooleanValue(parameter, CodegenConstants.IS_STRING_EXT_NAME);
        boolean isContainer = getBooleanValue(parameter, CodegenConstants.IS_CONTAINER_EXT_NAME);
        boolean isFile = getBooleanValue(parameter, CodegenConstants.IS_FILE_EXT_NAME);
        if (!isModel && !isPrimitiveType && !isDate && !isString && !isContainer && !isFile) {
            isModel = true;
        }
        parameter.vendorExtensions.put("x-is-model-type", isModel);
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Schema> definitions, OpenAPI openAPI) {
        CodegenOperation codegenOperation = super.fromOperation(path, httpMethod, operation, definitions, openAPI);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse response = findMethodResponse(operation.getResponses());

            if (response != null && response.getContent() != null && !response.getContent().isEmpty()) {

                final MediaType mediaType = new ArrayList<>(response.getContent().values()).get(0);

                if (mediaType.getSchema() != null) {
                    CodegenProperty cm = fromProperty("response", mediaType.getSchema());
                    codegenOperation.vendorExtensions.put("x-codegen-response", cm);
                    if(cm.datatype == "HttpContent") {
                        codegenOperation.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                    }
                }
            }
        }
        return codegenOperation;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        for (CodegenOperation op1 : operationList) {
            op1.vendorExtensions.put("x-has-uniq-produces", postProcessMediaTypes(op1.produces) == 1);
            op1.vendorExtensions.put("x-has-uniq-consumes", postProcessMediaTypes(op1.consumes) == 1);
            op1.vendorExtensions.put("x-has-notes", op1.notes.length() > 0);
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // Collect the model dependencies.
        List<Map<String, Object>> models = (List<Map<String, Object>>) objs.get("models");
        for (Map<String, Object> model : models) {
            Object v = model.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel m = (CodegenModel) v;
                List<String> d = new ArrayList<String>();
                for (CodegenProperty p : m.allVars) {
                    boolean isModel = false;
                    CodegenProperty item = p;

                    if (getBooleanValue(p, CodegenConstants.IS_CONTAINER_EXT_NAME)) {
                        item = p.items;
                    }
                    boolean isString = getBooleanValue(p, CodegenConstants.IS_STRING_EXT_NAME);
                    boolean isPrimitiveType = getBooleanValue(p, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME);
                    boolean isContainer = getBooleanValue(p, CodegenConstants.IS_CONTAINER_EXT_NAME);
                    boolean isInteger = getBooleanValue(p, CodegenConstants.IS_INTEGER_EXT_NAME);
                    if (item != null && !isString && !isPrimitiveType && !isContainer && !isInteger) {
                        if (!d.contains(item.datatype)) {
                            // LOGGER.info("Model " + m.name + " uses " + p.datatype);
                            d.add(item.datatype);
                            isModel = true;
                        }
                    }
                    p.vendorExtensions.put("x-is-model-type", isModel);
                }
                modelDepends.put(m.name, d);
                orderedModels.add(model);
            }
        }

        // Sort the models according to dependencies so that model that depend
        // on others appear at end of the list.
        final Map<String, List<String>> deps = modelDepends;
        Collections.sort(orderedModels, new Comparator<Map<String, Object>>() {
            @Override
            public int compare(Map<String, Object> lhs, Map<String, Object> rhs) {
                Object v = lhs.get("model");
                String lhsName = ((CodegenModel) v).name;
                v = rhs.get("model");
                String rhsName = ((CodegenModel) v).name;
                List<String> lhsList = deps.get(lhsName);
                List<String> rhsList = deps.get(rhsName);
                if (lhsList == rhsList) {
                    // LOGGER.info("First compare " + lhsName + "<" + rhsName);
                    return lhsName.compareTo(rhsName);
                }
                // Put models without dependencies first.
                if (lhsList == null) {
                    // LOGGER.info("  Empty " + lhsName + ", no check " + rhsName);
                    return -1;
                }
                if (rhsList == null) {
                    // LOGGER.info("  No check " + lhsName + ", empty " + rhsName);
                    return 1;
                }
                // Put models that depend on another after.
                if (lhsList.contains(rhsName)) {
                    // LOGGER.info("  LSH " + lhsName + " uses " + rhsName);
                    return 1;
                }
                if (rhsList.contains(lhsName)) {
                    // LOGGER.info("  RHS " + rhsName + " uses " + lhsName);
                    return -1;
                }
                // Put models with less dependencies first.
                if (lhsList.size() < rhsList.size()) {
                    // LOGGER.info("  LSH size " + lhsName + " < RHS size " + rhsName);
                    return -1;
                }
                if (lhsList.size() > rhsList.size()) {
                    // LOGGER.info("  LSH size " + lhsName + " > RHS size " + rhsName);
                    return 1;
                }
                // Sort models on their name.
                // LOGGER.info("Compare " + lhsName + "<" + rhsName);
                return lhsName.compareTo(rhsName);
            }
        });
        /* for (Map<String, Object> model : orderedModels) {
            Object v = model.get("model");
            if (v instanceof CodegenModel) {
                CodegenModel m = (CodegenModel) v;
                LOGGER.info("Order: " + m.name);
            }
        }*/
        return postProcessModelsEnum(objs);
    }
}
