/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.Collator;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ElmClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ElmClientCodegen.class);
    private Set<String> customPrimitives = new HashSet<String>();

    private static final String ELM_ENABLE_CUSTOM_BASE_PATHS = "elmEnableCustomBasePaths";
    private static final String ELM_ENABLE_HTTP_REQUEST_TRACKERS = "elmEnableHttpRequestTrackers";
    private static final String ENCODER = "elmEncoder";
    private static final String DECODER = "elmDecoder";
    private static final String DISCRIMINATOR_NAME = "discriminatorName";
    private static final String CUSTOM_TYPE = "elmCustomType";

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
        return "Generates a Elm client library (beta).";
    }

    public ElmClientCodegen() {
        super();
        outputFolder = "generated-code/elm";
        modelTemplateFiles.put("model.mustache", ".elm");
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

        customPrimitives = new HashSet<>(
                Arrays.asList(
                        "Date",
                        "DateTime",
                        "Uuid")
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
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("password", "String");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("file", "String");
        typeMapping.put("binary", "String");
        typeMapping.put("UUID", "Uuid");
        typeMapping.put("URI", "String");

        importMapping.clear();

        cliOptions.clear();
        final CliOption elmEnableCustomBasePaths = CliOption.newBoolean(ELM_ENABLE_CUSTOM_BASE_PATHS, "Enable setting the base path for each request");
        cliOptions.add(elmEnableCustomBasePaths);
        final CliOption elmEnableHttpRequestTrackers = CliOption.newBoolean(ELM_ENABLE_HTTP_REQUEST_TRACKERS, "Enable adding a tracker to each http request");
        cliOptions.add(elmEnableHttpRequestTrackers);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(ELM_ENABLE_CUSTOM_BASE_PATHS)) {
            final boolean enable = Boolean.TRUE.equals(Boolean.valueOf(additionalProperties.get(ELM_ENABLE_CUSTOM_BASE_PATHS).toString()));
            additionalProperties.put("enableCustomBasePaths", enable);
        }

        if (additionalProperties.containsKey(ELM_ENABLE_HTTP_REQUEST_TRACKERS)) {
            final boolean enable = Boolean.TRUE.equals(Boolean.valueOf(additionalProperties.get(ELM_ENABLE_HTTP_REQUEST_TRACKERS).toString()));
            additionalProperties.put("enableHttpRequestTrackers", enable);
        }

        if (StringUtils.isEmpty(System.getenv("ELM_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable ELM_POST_PROCESS_FILE not defined so the Elm code may not be properly formatted. To define it, try `export ELM_POST_PROCESS_FILE=\"/usr/local/bin/elm-format --elm-version={} --yes\"` (Linux/Mac)", "0.19");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        apiTemplateFiles.put("api.mustache", ".elm");
        supportingFiles.add(new SupportingFile("elm.mustache", "", "elm.json"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
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
        final String varName = camelize(name, true);
        return isReservedWord(varName) ? escapeReservedWord(name) : varName;
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        String camelized = camelize(value.replace(" ", "_").replace("(", "_").replace(")", "")); // TODO FIXME escape properly

        if (camelized.length() == 0) {
            LOGGER.error("Unable to determine enum variable name (name: {}, datatype: {}) from empty string. Default to UnknownEnumVariableName", value, datatype);
            camelized = "UnknownEnumVariableName";
        }

        if (!Character.isUpperCase(camelized.charAt(0))) {
            return "N" + camelized;
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
    public String escapeReservedWord(String name) {
        return name + "_";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + ("/src/" + apiPackage().replace('.', File.separatorChar)).replace("/", File.separator);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + ("/src/" + modelPackage().replace('.', File.separatorChar)).replace("/", File.separator);
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema) {
        CodegenModel m = super.fromModel(name, schema);

        if (ModelUtils.isArraySchema(schema)) {
            ArraySchema am = (ArraySchema) schema;
            CodegenProperty codegenProperty = fromProperty(name, (Schema) am.getItems());
            m.vendorExtensions.putAll(codegenProperty.vendorExtensions);
        }

        return m;
    }

    @SuppressWarnings({"static-method", "unchecked"})
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
            .collect(Collectors.toList());
        dataObj.put("models", models);
        dataObj.put("includeTime", true); // TODO only if used
        dataObj.put("includeUuid", true); // TODO only if used
        objects.put("Data", dataObj);
        return objects;
    }

    private ElmImport createImport(final String name) {
        final ElmImport elmImport = new ElmImport();
        final boolean isData = !customPrimitives.contains(name);
        final String modulePrefix = isData ? "Data." : "";
        elmImport.moduleName = modulePrefix + name;
        if (isData) {
            elmImport.as = name;
        }
        elmImport.exposures = new TreeSet<>();
        elmImport.exposures.add(name);
        elmImport.hasExposures = true;
        return elmImport;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(objs);
    }

    private static boolean anyOperationParam(final List<CodegenOperation> operations, final Predicate<CodegenParameter> predicate) {
        return operations.stream()
                .flatMap(operation -> Stream.of(
                        operation.bodyParams.stream(),
                        operation.queryParams.stream(),
                        operation.pathParams.stream(),
                        operation.headerParams.stream()
                ))
                .flatMap(a -> a)
                .filter(predicate)
                .findAny()
                .isPresent();
    }

    @Override
    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> allModels) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>) objs.get("operation");

        final Set<String> dependencies = new HashSet<>();

        for (CodegenOperation op : ops) {
            final List<Object> pathParams = Arrays.asList(op.path.substring(1).split("/")).stream()
                    .map(str -> {
                        if (str.startsWith("{") && str.endsWith("}")) {
                            return op.pathParams.stream().filter(p -> str.equals("{" + p.baseName + "}")).findFirst().orElse(null);
                        } else {
                            return "\"" + str + "\"";
                        }
                    })
                    .collect(Collectors.toList());
            op.vendorExtensions.put("pathParams", pathParams);

            for (CodegenParameter param : op.allParams) {
                if (param.isPrimitiveType || param.isContainer || param.isDate || param.isDateTime || param.isUuid) {
                    continue;
                }
                dependencies.add(param.dataType);
            }
            for (CodegenResponse resp : op.responses) {
                if (resp.primitiveType || resp.isMapContainer || resp.isDate || resp.isDateTime || resp.isUuid) {
                    continue;
                }
                dependencies.add(resp.dataType);
            }
        }

        final List<ElmImport> elmImports = new ArrayList<>();
        for (String key : dependencies) {
            final ElmImport elmImport = new ElmImport();
            elmImport.moduleName = "Data." + key;
            elmImport.as = key;
            elmImport.exposures = new HashSet<>();
            elmImport.exposures.add(key);
            elmImport.hasExposures = true;
            elmImports.add(elmImport);
        }
        final boolean hasDate = anyOperationParam(ops, param -> param.isDate);
        if (hasDate) {
            final ElmImport elmImport = new ElmImport();
            elmImport.moduleName = "DateOnly";
            elmImport.exposures = new TreeSet<>();
            elmImport.exposures.add("DateOnly");
            elmImport.hasExposures = true;
            elmImports.add(elmImport);
        }
        final boolean hasDateTime = anyOperationParam(ops, param -> param.isDateTime);
        if (hasDateTime) {
            final ElmImport elmImport = new ElmImport();
            elmImport.moduleName = "DateTime";
            elmImport.exposures = new TreeSet<>();
            elmImport.exposures.add("DateTime");
            elmImport.hasExposures = true;
            elmImports.add(elmImport);
        }
        final boolean hasUuid = anyOperationParam(ops, param -> param.isUuid);
        if (hasUuid) {
            final ElmImport elmImport = new ElmImport();
            elmImport.moduleName = "Uuid";
            elmImport.exposures = new TreeSet<>();
            elmImport.exposures.add("Uuid");
            elmImport.hasExposures = true;
            elmImports.add(elmImport);
        }
        operations.put("elmImports", elmImports);

        return operations;
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

    private Optional<String> paramToStringMapper(final String paramName, final CodegenProperty property) {
        if (property.isEnum) {
            return Optional.of(toVarName(paramName) + "ToString");
        } else if (property.isString || property.isBinary || property.isByteArray) {
            return Optional.empty();
        } else if (property.isBoolean) {
            return Optional.of("(\\val -> if val then \"true\" else \"false\")");
        } else if (property.isDateTime) {
            return Optional.of("DateTime.toString");
        } else if (property.isDate) {
            return Optional.of("DateOnly.toString");
        } else if (property.isUuid) {
            return Optional.of("Uuid.toString");
        } else if (property.isInteger || property.isLong) {
            return Optional.of("String.fromInt");
        } else if (property.isFloat || property.isDouble) {
            return Optional.of("String.fromFloat");
        } else {
            return Optional.of(property.dataType + ".toString");
        }
    }

    private CodegenProperty paramToProperty(final CodegenParameter parameter) {
        final CodegenProperty property = new CodegenProperty();
        property.dataType = parameter.dataType;
        property.isEnum = parameter.isEnum;
        property.isString = parameter.isString;
        property.isBinary = parameter.isBinary;
        property.isByteArray = parameter.isByteArray;
        property.isBoolean = parameter.isBoolean;
        property.isDateTime = parameter.isDateTime;
        property.isDate = parameter.isDate;
        property.isUuid = parameter.isUuid;
        property.isInteger = parameter.isInteger;
        property.isLong = parameter.isLong;
        property.isFloat = parameter.isFloat;
        property.isDouble = parameter.isDouble;
        return property;
    }

    private String paramToString(final String prefix, final CodegenParameter param, final boolean useMaybe, final String maybeMapResult) {
        final String paramName = prefix + "." + param.paramName;
        if (!useMaybe) {
            param.required = true;
        }

        final String mapFn = param.isListContainer
                ? "(String.join \",\"" + paramToStringMapper(param.paramName, param.items).map(mapper -> " << List.map " + mapper).orElse("") + ")"
                : paramToStringMapper(param.paramName, paramToProperty(param)).orElse("");

        String mapResult = "";
        if (maybeMapResult != null) {
            if ("".equals(mapFn)) {
                mapResult = maybeMapResult;
            } else {
                mapResult = maybeMapResult + (param.required ? " <|" : " <<");
            }
        }
        final String just = useMaybe ? "Just (" : "";
        final String justEnd = useMaybe ? ")" : "";
        return (param.required ? just : "Maybe.map (") + mapResult + " " + mapFn + (param.required ? " " : ") ") + paramName + (param.required ? justEnd : "");
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

    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        final CodegenProperty property = super.fromProperty(name, p);

        if (property.isEnum) {
            addEncoderAndDecoder(property.vendorExtensions, property.baseName, DataTypeExposure.INTERNAL);
            property.vendorExtensions.put(CUSTOM_TYPE, property.datatypeWithEnum);
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
        if (!vendorExtensions.containsKey(ENCODER)) {
            vendorExtensions.put(ENCODER, encodeName);
        }
        if (!vendorExtensions.containsKey(DECODER)) {
            vendorExtensions.put(DECODER, decoderName);
        }
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
        }
    }
}
