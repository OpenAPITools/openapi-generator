package org.openapitools.codegen.languages;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;

import java.io.File;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.common.collect.Maps;

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

        this.generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata).stability(Stability.EXPERIMENTAL)
                .build();

        // Pydantic has json and schema as property name on BaseModel
        reservedWords.addAll(Arrays.asList("json", "schema"));

        // clear import mapping (from default generator) as python does not use it
        // at the moment
        importMapping.clear();

        typeMapping.put("object", "Dict");
        typeMapping.put("AnyType", "Any");
        typeMapping.put("file", "bytes");

        modifyFeatureSet(features -> features.documentationFeatures(null)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON)));

        // TODO: remove?
        // super.processOpts();

        supportsInheritance = true;
        modelPackage = packageName + "." + "model";
        apiPackage = packageName + "." + "api";
        outputFolder = "generated-code" + File.separatorChar + "python-uplink";

        languageSpecificPrimitives.add("List");
        languageSpecificPrimitives.add("Dict");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Dict");

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

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return name + "_";
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
                        if (!isUnionType(importModelName)) {
                            cm.imports.add(toModelImport(importModelName));
                        }
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
    protected String getParameterDataType(Parameter parameter, Schema p) {
        // handle enums of various data types
        Schema inner;
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema mp1 = (ArraySchema) p;
            inner = mp1.getItems();
            return this.getSchemaType(p) + "[" + this.getParameterDataType(parameter, inner) + "]";
        }

        return this.getTypeDeclaration(p);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "[str, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    /**
     * Extracts the list of type names from a list of schemas. Excludes `AnyType` if
     * there are other valid types extracted.
     *
     * @param schemas list of schemas
     * @return list of types
     */
    protected List<String> getTypesFromSchemas(List<Schema> schemas) {
        List<Schema> filteredSchemas = schemas.size() > 1 ? schemas.stream()
                .filter(schema -> !"AnyType".equals(super.getSchemaType(schema))).collect(Collectors.toList())
                : schemas;

        return filteredSchemas.stream().map(schema -> {
            String schemaType = getSchemaType(schema);
            if (ModelUtils.isArraySchema(schema)) {
                ArraySchema ap = (ArraySchema) schema;
                Schema inner = ap.getItems();
                schemaType = schemaType + "[" + getSchemaType(inner) + "]";
            }
            return schemaType;
        }).distinct().collect(Collectors.toList());
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);

        if (isReservedWord(openAPIType)) {
            return toModelName(openAPIType);
        }

        return openAPIType;
    }

    @Override
    public String toAnyOfName(List<String> names, ComposedSchema composedSchema) {
        List<String> types = getTypesFromSchemas(composedSchema.getAnyOf());

        return "Union[" + String.join(", ", types) + "]";
    }

    @Override
    public String toOneOfName(List<String> names, ComposedSchema composedSchema) {
        List<String> types = getTypesFromSchemas(composedSchema.getOneOf());

        return "Union[" + String.join(", ", types) + "]";
    }

    @Override
    public String toRegularExpression(String pattern) {
        String regex = super.toRegularExpression(pattern);
        return StringUtils.substring(regex, 1, -1);
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
        if (isUnionType(name)) {
            return name;
        }

        return "from " + modelPackage() + "." + toModelFilename(name) + " import " + toModelName(name);
    }

    private boolean isUnionType(String name) {
        return name.startsWith("Union[");
    }
}
