/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Mustache.Lambda;
import com.samskivert.mustache.Template;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.utils.OnceLogger.once;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ElmClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ElmClientCodegen.class);
<<<<<<< HEAD
=======
    private Set<String> customPrimitives = new HashSet<String>();
    private ElmVersion elmVersion = ElmVersion.ELM_019;
    private Boolean elmPrefixCustomTypeVariants = false;

    private static final String ELM_VERSION = "elmVersion";
    private static final String ELM_PREFIX_CUSTOM_TYPE_VARIANTS = "elmPrefixCustomTypeVariants";
    private static final String ELM_ENABLE_CUSTOM_BASE_PATHS = "elmEnableCustomBasePaths";
    private static final String ELM_ENABLE_HTTP_REQUEST_TRACKERS = "elmEnableHttpRequestTrackers";
    private static final String ENCODER = "elmEncoder"; // TODO: 5.0 Remove
    private static final String VENDOR_EXTENSION_ENCODER = "x-elm-encoder";
    private static final String DECODER = "elmDecoder"; // TODO: 5.0 Remove
    private static final String VENDOR_EXTENSION_DECODER = "x-elm-decoder";
    private static final String DISCRIMINATOR_NAME = "discriminatorName"; // TODO: 5.0 Remove
    private static final String VENDOR_EXTENSION_DISCRIMINATOR_NAME = "x-discriminator-name";
    private static final String CUSTOM_TYPE = "elmCustomType"; // TODO: 5.0 Remove
    private static final String VENDOR_EXTENSION_CUSTOM_TYPE = "x-elm-custom-type";
>>>>>>> origin/master

    protected String packageName = "openapi";
    protected String packageVersion = "1.0.0";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "elm";
    }

    public String getHelp() {
        return "Generates an Elm client library.";
    }

    public ElmClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.of(SecurityFeature.BearerToken))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath
                )
        );

        templateDir = "elm";
        apiPackage = "Api.Request";
        modelPackage = "Api";

        supportsInheritance = true;

        reservedWords = new HashSet<>(
                Arrays.asList(
                        "if", "then", "else",
                        "case", "of",
                        "let", "in",
                        "type",
                        "module", "where",
                        "import", "exposing",
                        "as",
                        "port")
        );

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "Order",
                        "Never",
                        "List",
                        "Maybe",
                        "Result",
                        "Program",
                        "Cmd",
                        "Sub")
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "Bool",
                        "Dict",
                        "Float",
                        "Int",
                        "List",
                        "String")
        );

        instantiationTypes.clear();
        instantiationTypes.put("array", "List");
        instantiationTypes.put("map", "Dict");

        typeMapping.clear();
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Int");
        typeMapping.put("number", "Float");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Float");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "String");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Dict");
        typeMapping.put("date", "Posix");
        typeMapping.put("DateTime", "Posix");
        typeMapping.put("password", "String");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("file", "String");
        typeMapping.put("binary", "String");
        typeMapping.put("UUID", "Uuid");
        typeMapping.put("URI", "String");

        importMapping.clear();

        cliOptions.clear();

        apiTemplateFiles.put("operation.mustache", ".elm");
        modelTemplateFiles.put("model.mustache", ".elm");
        supportingFiles.add(new SupportingFile("Api.mustache", "", "src" + File.separator + "Api.elm"));
        supportingFiles.add(new SupportingFile("Time.mustache", "", "src" + File.separator + "Api" + File.separator + "Time.elm"));
        supportingFiles.add(new SupportingFile("elm.mustache", "", "elm.json"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
            .put("removeWhitespace", new RemoveWhitespaceLambda());
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("\"", "");
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "Default";
        }
        return camelize(name);
    }

    @Override
    public String toModelName(String name) {
        final String modelName = camelize(name);
        return defaultIncludes.contains(modelName) ? modelName + "_" : modelName;
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return toModelName(property.name);
    }

    @Override
    public String toVarName(String name) {
        final String varName = camelize(name.replaceAll("[^a-zA-Z0-9_]", ""), true);
        return isReservedWord(varName) ? escapeReservedWord(name) : varName;
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        String camelized = camelize(value.replace(" ", "_").replace("(", "_").replace(")", "")); // TODO FIXME escape properly
        if (camelized.length() == 0) {
            LOGGER.error("Unable to determine enum variable name (name: {}, datatype: {}) from empty string. Default to UnknownEnumVariableName", value, datatype);
            camelized = "UnknownEnumVariableName";
        }
        return camelized;
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            return instantiationTypes.get("array") + " " + inner;
        } else {
            return null;
        }
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + "src" + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + "src" + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String escapeReservedWord(String name) {
        return name + "_";
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if (property.getAllowableValues() != null && !property.getAllowableValues().isEmpty()) {
            property.isModel = true;
        }
    }

    @SuppressWarnings({"static-method", "unchecked"})
<<<<<<< HEAD
    public Map<String, Object> postProcessAllModels(final Map<String, Object> orgObjs) {
        final Map<String, Object> objs = super.postProcessAllModels(orgObjs);

        // put all models in one file
        final Map<String, Object> objects = new HashMap<>();
        final Map<String, Object> dataObj = objs.values().stream()
            .map(obj -> (Map<String, Object>) obj)
            .findFirst()
            .orElse(new HashMap<>());
        final List<Map<String, Object>> models = objs.values().stream()
            .map(obj -> (Map<String, Object>) obj)
            .flatMap(obj -> ((List<Map<String, Object>>) obj.get("models")).stream())
            .flatMap(obj -> {
                final CodegenModel model = (CodegenModel) obj.get("model");
                // circular references
                model.vars.forEach(var -> {
                    var.isCircularReference = model.allVars.stream()
                        .filter(v -> var.baseName.equals(v.baseName))
                        .map(v -> v.isCircularReference)
                        .findAny()
                        .orElse(false);
                    CodegenProperty items = var.items;
                    while (items != null) {
                        items.isCircularReference = var.isCircularReference;
                        items.required = true;
                        items = items.items;
                    }
                });
                // discriminators
                if (model.discriminator != null && model.getChildren() != null) {
                    model.getChildren().forEach(child -> {
                        child.allOf = child.allOf.stream()
                            .map(v -> model.classname.equals(v) ? "Base" + v : v)
                            .collect(Collectors.toSet());
                    });
=======
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {

        // TODO: 5.0: Remove the camelCased vendorExtension below and ensure templates use the newer property naming.
        once(LOGGER).warn("4.3.0 has deprecated the use of vendor extensions which don't follow lower-kebab casing standards with x- prefix.");

        // Index all CodegenModels by model name.
        Map<String, CodegenModel> allModels = new HashMap<>();
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            String modelName = toModelName(entry.getKey());
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                allModels.put(modelName, cm);
            }
        }
        // Let parent know about all its children
        for (CodegenModel cm : allModels.values()) {
            CodegenModel parent = allModels.get(cm.parent);

            if (parent != null) {
                if (parent.children == null) {
                    parent.children = new ArrayList<>();
                    parent.hasChildren = true;
                }
                parent.children.add(cm);
                Collections.sort(parent.children, new Comparator<CodegenModel>() {
                    @Override
                    public int compare(CodegenModel cm1, CodegenModel cm2) {
                        return Collator.getInstance(Locale.ROOT).compare(cm1.classname, cm2.classname);
                    }
                });
            }
        }
        setCircularReferences(allModels);
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                if (cm.isEnum) {
                    addEncoderAndDecoder(cm.vendorExtensions, cm.classname, DataTypeExposure.EXPOSED);
                    cm.vendorExtensions.put(CUSTOM_TYPE, cm.classname); // TODO: 5.0 Remove
                    cm.vendorExtensions.put(VENDOR_EXTENSION_CUSTOM_TYPE, cm.classname);
                } else if (cm.isAlias) {
                    addEncoderAndDecoder(cm.vendorExtensions, cm.dataType, DataTypeExposure.EXPOSED);
                }

                List<ElmImport> elmImports = new ArrayList<>();
                for (CodegenProperty property : cm.allVars) {
                    if (property.complexType != null) {
                        final ElmImport elmImport = createImport(property.complexType);
                        elmImports.add(elmImport);
                    }
>>>>>>> origin/master
                }
                // remove *AllOf
                if (model.classname.endsWith("AllOf")) {
                    return Stream.empty();
                } else {
                    model.allOf.removeIf(name -> name.endsWith("AllOf"));
                    return Stream.of(obj);
                }
<<<<<<< HEAD
            })
            .collect(Collectors.toList());

        final boolean includeTime = anyVarMatches(models, prop -> prop.isDate || prop.isDateTime);
        final boolean includeUuid = anyVarMatches(models, prop -> prop.isUuid);

        dataObj.put("models", models);
        dataObj.put("includeTime", includeTime);
        dataObj.put("includeUuid", includeUuid);
        objects.put("Data", dataObj);
        return objects;
    }

    private boolean anyVarMatches(final List<Map<String, Object>> models, final Predicate<CodegenProperty> predicate) {
        return models.stream()
            .map(obj -> (CodegenModel) obj.get("model"))
            .flatMap(model -> model.vars.stream())
            .filter(var -> {
                CodegenProperty prop = var;
                while (prop != null) {
                    if (predicate.test(prop)) {
                        return true;
=======
                if (cm.discriminator != null && cm.children != null) {
                    for (CodegenModel child : cm.children) {
                        // add child imports
                        final ElmImport elmImport = createImport(child.classname);
                        elmImports.add(elmImport);

                        final String propertyName = cm.discriminator.getPropertyName();
                        final List<CodegenProperty> allVars = child.allVars.stream()
                                .filter(var -> !var.baseName.equals(propertyName))
                                .collect(Collectors.toList());
                        child.allVars.clear();
                        child.allVars.addAll(allVars);

                        child.vendorExtensions.put(DISCRIMINATOR_NAME, propertyName); // TODO: 5.0 Remove
                        child.vendorExtensions.put(VENDOR_EXTENSION_DISCRIMINATOR_NAME, propertyName);
>>>>>>> origin/master
                    }
                    prop = prop.items;
                }
                return false;
            })
            .count() > 0;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(objs);
    }

    private static boolean anyOperationParam(final List<CodegenOperation> operations, final Predicate<CodegenParameter> predicate) {
        return operations.stream()
                .flatMap(operation -> operation.allParams.stream())
                .filter(predicate)
                .findAny()
                .isPresent();
    }

    private static boolean anyOperationResponse(final List<CodegenOperation> operations, final Predicate<CodegenResponse> predicate) {
        return operations.stream()
                .flatMap(operation -> operation.responses.stream())
                .filter(predicate)
                .findAny()
                .isPresent();
    }

    @Override
    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> allModels) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>) objs.get("operation");
        ops.forEach(op -> {
            op.allParams = op.allParams.stream().sorted(new ParameterSorter()).collect(Collectors.toList());
            op.responses.forEach(response -> {
                if (response.isDefault) {
                    response.isModel = !response.primitiveType;
                }
            });
        });

        final boolean includeTime =
            anyOperationResponse(ops, response -> response.isDate || response.isDateTime) ||
            anyOperationParam(ops, param -> param.isDate || param.isDateTime);
        final boolean includeUuid =
            anyOperationResponse(ops, response -> response.isUuid) ||
            anyOperationParam(ops, param -> param.isUuid);
        operations.put("includeTime", includeTime);
        operations.put("includeUuid", includeUuid);

<<<<<<< HEAD
        return operations;
    }

    static class ParameterSorter implements Comparator<CodegenParameter> {
        public int compare(final CodegenParameter p1, final CodegenParameter p2) { 
            return index(p1) - index(p2);
        }
=======
        // TODO: 5.0: Remove the camelCased vendorExtension below and ensure templates use the newer property naming.
        once(LOGGER).warn("4.3.0 has deprecated the use of vendor extensions which don't follow lower-kebab casing standards with x- prefix.");

        final Set<String> dependencies = new HashSet<>();

        for (CodegenOperation op : ops) {
            if (ElmVersion.ELM_018.equals(elmVersion)) { // elm 0.18
                String path = op.path;
                for (CodegenParameter param : op.pathParams) {
                    final String var = paramToString("params", param, false, null);
                    path = path.replace("{" + param.baseName + "}", "\" ++ " + var + " ++ \"");
                }
                op.path = ("\"" + path + "\"").replaceAll(" \\+\\+ \"\"", "");
            } else { // elm 0.19 or later
                final List<Object> pathParams = Arrays.asList(op.path.substring(1).split("/")).stream()
                        .map(str -> {
                            if (str.startsWith("{") && str.endsWith("}")) {
                                return op.pathParams.stream().filter(p -> str.equals("{" + p.baseName + "}")).findFirst().orElse(null);
                            } else {
                                return "\"" + str + "\"";
                            }
                        })
                        .collect(Collectors.toList());
                op.vendorExtensions.put("pathParams", pathParams); // TODO: 5.0 Remove
                op.vendorExtensions.put("x-path-params", pathParams);
            }
>>>>>>> origin/master

        private int index(final CodegenParameter p) {
            if (p.isPathParam) {
                return 1;
            }
            if (p.isQueryParam) {
                return 2;
            }
            if (p.isHeaderParam) {
                return 3;
            }
            if (p.isBodyParam) {
                return 4;
            }
            return 5;
        }
    }   
    
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return Boolean.valueOf(p.getDefault().toString()) ? "True" : "False";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        }
        return null;
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else
            type = openAPIType;
        return toModelName(type);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getTypeDeclaration(inner);
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return getTypeDeclaration(inner);
        }
        return super.getTypeDeclaration(p);
    }

<<<<<<< HEAD
    private static class RemoveWhitespaceLambda implements Mustache.Lambda {
        @Override
        public void execute(final Template.Fragment fragment, final Writer writer) throws IOException {
            writer.write(fragment.execute().replaceAll("\\s+", ""));
=======
    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        final CodegenProperty property = super.fromProperty(name, p);

        // TODO: 5.0: Remove the camelCased vendorExtension below and ensure templates use the newer property naming.
        once(LOGGER).warn("4.3.0 has deprecated the use of vendor extensions which don't follow lower-kebab casing standards with x- prefix.");

        if (property.isEnum) {
            addEncoderAndDecoder(property.vendorExtensions, property.baseName, DataTypeExposure.INTERNAL);
            property.vendorExtensions.put(CUSTOM_TYPE, property.datatypeWithEnum); // TODO: 5.0 Remove
            property.vendorExtensions.put(VENDOR_EXTENSION_CUSTOM_TYPE, property.datatypeWithEnum);
        } else {
            final boolean isPrimitiveType = property.isMapContainer ? isPrimitiveDataType(property.dataType) : property.isPrimitiveType;
            addEncoderAndDecoder(property.vendorExtensions, property.dataType, isPrimitiveType ? DataTypeExposure.PRIMITIVE : DataTypeExposure.EXTERNAL);
        }

        return property;
    }

    @Override
    public CodegenResponse fromResponse(String responseCode, ApiResponse resp) {
        final CodegenResponse response = super.fromResponse(responseCode, resp);
        if (response.dataType != null) {
            final boolean isPrimitiveType = response.isMapContainer ? isPrimitiveDataType(response.dataType) : response.primitiveType;
            addEncoderAndDecoder(response.vendorExtensions, response.dataType, isPrimitiveType ? DataTypeExposure.PRIMITIVE : DataTypeExposure.EXTERNAL);
        }
        return response;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        final boolean isPrimitiveType = parameter.isMapContainer ? isPrimitiveDataType(parameter.dataType) : parameter.isPrimitiveType;
        addEncoderAndDecoder(parameter.vendorExtensions, parameter.dataType, isPrimitiveType ? DataTypeExposure.PRIMITIVE : DataTypeExposure.EXTERNAL);
    }

    @Override
    public void updateCodegenPropertyEnum(final CodegenProperty property) {
        super.updateCodegenPropertyEnum(property);
        if (!elmPrefixCustomTypeVariants) {
            return;
        }

        final Map<String, Object> allowableValues = property.allowableValues;
        if (allowableValues == null) {
            return;
        }

        final List<Map<String, Object>> enumVars = (ArrayList<Map<String, Object>>) allowableValues.get("enumVars");
        if (enumVars == null) {
            return;
        }
        final String prefix = toEnumName(property);
        for (Map<String, Object> enumVar : enumVars) {
            if (!enumVar.containsKey("_isPrefixed")) {
                enumVar.put("name", prefix + enumVar.get("name"));
                enumVar.put("_isPrefixed", true);
            }
        }
    }

    private boolean isPrimitiveDataType(String dataType) {
        return languageSpecificPrimitives.contains(dataType);
    }

    private void addEncoderAndDecoder(final Map<String, Object> vendorExtensions, final String dataType, final DataTypeExposure dataTypeExposure) {
        final String baseName = org.openapitools.codegen.utils.StringUtils.camelize(dataType, true);
        String encodeName;
        String decoderName;
        switch (dataTypeExposure) {
            case EXPOSED:
                decoderName = "decoder";
                encodeName = "encode";
                break;
            case INTERNAL:
                encodeName = "encode" + StringUtils.capitalize(baseName);
                decoderName = baseName + "Decoder";
                break;
            case EXTERNAL:
                encodeName = dataType + ".encode";
                decoderName = dataType + ".decoder";
                break;
            case PRIMITIVE:
                encodeName = "Encode." + baseName;
                decoderName = "Decode." + baseName;
                break;
            default:
                encodeName = "";
                decoderName = "";
        }

        vendorExtensions.putIfAbsent(ENCODER, encodeName); // TODO: 5.0 Remove
        vendorExtensions.putIfAbsent(VENDOR_EXTENSION_ENCODER, encodeName);

        vendorExtensions.putIfAbsent(DECODER, decoderName); // TODO: 5.0 Remove
        vendorExtensions.putIfAbsent(VENDOR_EXTENSION_DECODER, decoderName);
    }

    private enum DataTypeExposure {
        EXPOSED,
        INTERNAL,
        EXTERNAL,
        PRIMITIVE
    }

    private static class ElmImport {
        public String moduleName;
        public String as;
        public Set<String> exposures;
        public Boolean hasExposures;
    }

    private enum ElmVersion {
        ELM_018,
        ELM_019
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        String elmPostProcessFile = System.getenv("ELM_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(elmPostProcessFile)) {
            return; // skip if ELM_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with elm extension
        if ("elm".equals(FilenameUtils.getExtension(file.toString()))) {
            // e.g. elm-format -w yourcode.elm
            String command = elmPostProcessFile + " " + file.toString();

            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit code: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: " + command);
                }
            } catch (Exception e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
            }
>>>>>>> origin/master
        }
    }
}
