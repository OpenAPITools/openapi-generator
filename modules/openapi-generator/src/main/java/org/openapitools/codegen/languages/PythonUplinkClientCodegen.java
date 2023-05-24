package org.openapitools.codegen.languages;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.model.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import static org.openapitools.codegen.utils.StringUtils.underscore;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PythonUplinkClientCodegen extends AbstractPythonCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    static final Logger LOGGER = LoggerFactory.getLogger(PythonUplinkClientCodegen.class);

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

        embeddedTemplateDir = templateDir = "python-uplink";

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

        // Data types of the above values which are automatically imported
        defaultIncludes = Sets.newHashSet("Union");

        modifyFeatureSet(features -> features.documentationFeatures(null)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON)));

        supportsInheritance = true;

        languageSpecificPrimitives.add("List");
        languageSpecificPrimitives.add("Dict");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Dict");

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }

        // add the models and apis folders
        supportingFiles.add(new SupportingFile("__init__.mustache", packagePath(), "__init__.py"));
        supportingFiles.add(new SupportingFile("client.mustache", packagePath(), "client.py"));
        supportingFiles.add(new SupportingFile("__init__model.mustache", packagePath() + File.separatorChar + "model",
        "__init__.py"));
        supportingFiles.add(
            new SupportingFile("__init__api.mustache", packagePath() + File.separatorChar + "api", "__init__.py"));
        supportingFiles.add(new SupportingFile("uplink_util.mustache", packagePath(), "uplink_util.py"));

        modelTemplateFiles.put("model.mustache", ".py");
        apiTemplateFiles.put("api.mustache", ".py");

        modelPackage = packageName + "." + "model";
        apiPackage = packageName + "." + "api";
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
     * @param objs a map going from the model name to a object holding the model
     *                info
     * @return the updated objs
     */
    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);

        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        for (String schemaName : allDefinitions.keySet()) {
            String modelName = toModelName(schemaName);

            ModelsMap objModel = objs.get(modelName);
            if (objModel != null) { // to avoid form parameter's models that are not generated (skipFormModel=true)
                List<ModelMap> models = objModel.getModels();
                for (ModelMap model : models) {
                    CodegenModel cm = model.getModel();

                    // Transform imports
                    String[] importModelNames = cm.imports.toArray(new String[0]);
                    cm.imports.clear();
                    for (String importModelName : importModelNames) {
                        if (!isUnionType(importModelName)) {
                            cm.imports.add(toModelImport(importModelName));
                        }
                    }

                    // set isAlias if we need to add an alias
                    for (CodegenProperty var : cm.vars) {
                        if (!var.name.equals(var.baseName)) {
                            var.vendorExtensions.put("x-has-custom-name", true);
                        }
                    }
                    // set isAlias if we need to add an alias
                    for (CodegenProperty var : cm.requiredVars) {
                        if (!var.name.equals(var.baseName)) {
                            var.vendorExtensions.put("x-has-custom-name", true);
                        }
                    }
                    // set isAlias if we need to add an alias
                    for (CodegenProperty var : cm.optionalVars) {
                        if (!var.name.equals(var.baseName)) {
                            var.vendorExtensions.put("x-has-custom-name", true);
                        }
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        @SuppressWarnings("unchecked")
        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();

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

        return objs;
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
            // name = name.replaceFirst("Union[", "");
            // name = name.replaceAll("]", "");

            return name;
        }

        // For some reason the model import is sometimes already an import
        // This should prevent creating an import statement that imports an import
        // statement
        if (name.startsWith("from " + packageName)) {
            return name;
        }

        return "from " + modelPackage() + "." + toModelFilename(name) + " import " + toModelName(name);
    }

    /**
     * Maps the fully qualified model import to the initial given name. In case of
     * union types the map will have multiple entries. For example for "classA |
     * classB" the map will two entries have ["model.classA","classA"] and
     * ["model.classB","classB"].
     *
     * @param name the name of the "Model"
     * @return Map between the fully qualified model import and the initial given
     *         name.
     */
    @Override
    public Map<String, String> toModelImportMap(String name) {
        return toImportMap(splitComposedType(name));
    }

    private String[] splitComposedType(String name) {
        String[] split = name.replace(" ", "").split("[\\[\\],]");
        return split;
    }

    private Map<String, String> toImportMap(String... names) {
        Map<String, String> result = Maps.newHashMap();
        for (String name : names) {
            if (needToImport(name)) {
                result.put(toModelImport(name), name);
            }
        }
        return result;
    }

    private boolean isUnionType(String name) {
        return name.startsWith("Union[");
    }

    @Override
    public String toApiVarName(String name) {
        if (name.length() == 0) {
            return "default_api";
        }

        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // e.g. PhoneNumberApi.py => phone_number_api.py
        return underscore(name);
    }
}
