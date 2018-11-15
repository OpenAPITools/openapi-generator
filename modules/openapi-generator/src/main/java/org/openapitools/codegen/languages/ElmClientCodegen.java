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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.ModelUtils;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.io.FilenameUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class ElmClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ElmClientCodegen.class);
    private Set<String> customPrimitives = new HashSet<String>();
    private ElmVersion elmVersion = ElmVersion.ELM_019;
    private Boolean elmPrefixCustomTypeVariants = false;

    private static final String ELM_VERSION = "elmVersion";
    private static final String ELM_PREFIX_CUSTOM_TYPE_VARIANTS = "elmPrefixCustomTypeVariants";
    private static final String ENCODER = "elmEncoder";
    private static final String DECODER = "elmDecoder";
    private static final String DISCRIMINATOR_NAME = "discriminatorName";
    private static final String UNION_TYPE = "elmUnionType";

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
        apiTemplateFiles.put("api.mustache", ".elm");
        templateDir = "elm";

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
                        "String")
        );

        customPrimitives = new HashSet<>(
                Arrays.asList(
                        "Byte",
                        "DateOnly",
                        "DateTime")
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
        typeMapping.put("date", "DateOnly");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("password", "String");
        typeMapping.put("ByteArray", "Byte");
        typeMapping.put("file", "String");
        typeMapping.put("binary", "String");

        importMapping.clear();

        cliOptions.clear();

        final CliOption elmVersion = new CliOption(ELM_VERSION, "Elm version: 0.18, 0.19").defaultValue("0.19");
        final Map<String, String> supportedVersions = new HashMap<>();
        supportedVersions.put("0.18", "Elm 0.18");
        supportedVersions.put("0.19", "Elm 0.19");
        elmVersion.setEnum(supportedVersions);
        cliOptions.add(elmVersion);
        final CliOption elmPrefixCustomTypeVariants = CliOption.newBoolean(ELM_PREFIX_CUSTOM_TYPE_VARIANTS, "Prefix custom type variants");
        cliOptions.add(elmPrefixCustomTypeVariants);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(ELM_VERSION)) {
            final String version = (String) additionalProperties.get(ELM_VERSION);
            if ("0.18".equals(version)) {
                elmVersion = ElmVersion.ELM_018;
            } else {
                elmVersion = ElmVersion.ELM_019;
            }
        }

        if (additionalProperties.containsKey(ELM_PREFIX_CUSTOM_TYPE_VARIANTS)) {
            elmPrefixCustomTypeVariants = Boolean.TRUE.equals(Boolean.valueOf(additionalProperties.get(ELM_PREFIX_CUSTOM_TYPE_VARIANTS).toString()));
        }

        if (StringUtils.isEmpty(System.getenv("ELM_POST_PROCESS_FILE"))) {
            if (elmVersion.equals(ElmVersion.ELM_018)) { // 0.18
                LOGGER.info("Environment variable ELM_POST_PROCESS_FILE not defined so the Elm code may not be properly formatted. To define it, try `export ELM_POST_PROCESS_FILE=\"/usr/local/bin/elm-format --elm-version={} --yes\"` (Linux/Mac)", "0.18");
            } else { // 0.19
                LOGGER.info("Environment variable ELM_POST_PROCESS_FILE not defined so the Elm code may not be properly formatted. To define it, try `export ELM_POST_PROCESS_FILE=\"/usr/local/bin/elm-format --elm-version={} --yes\"` (Linux/Mac)", "0.19");
            }
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        switch (elmVersion) {
            case ELM_018:
                LOGGER.info("Elm version: 0.18");
                additionalProperties.put("isElm018", true);
                supportingFiles.add(new SupportingFile("DateOnly018.mustache", "src", "DateOnly.elm"));
                supportingFiles.add(new SupportingFile("DateTime018.mustache", "src", "DateTime.elm"));
                supportingFiles.add(new SupportingFile("elm-package018.mustache", "", "elm-package.json"));
                supportingFiles.add(new SupportingFile("Main018.mustache", "src", "Main.elm"));
                break;
            case ELM_019:
                LOGGER.info("Elm version: 0.19");
                additionalProperties.put("isElm019", true);
                supportingFiles.add(new SupportingFile("DateOnly.mustache", "src", "DateOnly.elm"));
                supportingFiles.add(new SupportingFile("DateTime.mustache", "src", "DateTime.elm"));
                supportingFiles.add(new SupportingFile("elm.mustache", "", "elm.json"));
                supportingFiles.add(new SupportingFile("Main.mustache", "src", "Main.elm"));
                break;
            default:
                throw new RuntimeException("Undefined Elm version");
        }

        supportingFiles.add(new SupportingFile("Byte.mustache", "src", "Byte.elm"));
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
        return initialCaps(name);
    }

    @Override
    public String toModelName(String name) {
        final String modelName = org.openapitools.codegen.utils.StringUtils.camelize(name);
        return defaultIncludes.contains(modelName) ? modelName + "_" : modelName;
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return toModelName(property.name);
    }

    @Override
    public String toVarName(String name) {
        final String varName = org.openapitools.codegen.utils.StringUtils.camelize(name, true);
        return isReservedWord(varName) ? escapeReservedWord(name) : varName;
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        String camelized = org.openapitools.codegen.utils.StringUtils.camelize(value.replace(" ", "_").replace("(", "_").replace(")", "")); // TODO FIXME escape properly

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
        return outputFolder + "/src/Request/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/src/Data/" + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema, Map<String, Schema> allDefinitions) {
        CodegenModel m = super.fromModel(name, schema, allDefinitions);

        if (ModelUtils.isArraySchema(schema)) {
            ArraySchema am = (ArraySchema) schema;
            CodegenProperty codegenProperty = fromProperty(name, (Schema) am.getItems());
            m.vendorExtensions.putAll(codegenProperty.vendorExtensions);
        }

        return m;
    }

    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
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
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                if (cm.isEnum) {
                    addEncoderAndDecoder(cm.vendorExtensions, cm.classname, DataTypeExposure.EXPOSED);
                    cm.vendorExtensions.put(UNION_TYPE, cm.classname);
                } else if (cm.isAlias) {
                    addEncoderAndDecoder(cm.vendorExtensions, cm.dataType, DataTypeExposure.EXPOSED);
                }

                List<ElmImport> elmImports = new ArrayList<>();
                for (CodegenProperty property : cm.allVars) {
                    if (property.complexType != null) {
                        final ElmImport elmImport = createImport(property.complexType);
                        elmImports.add(elmImport);
                    }
                }
                if (cm.isArrayModel) {
                    if (cm.arrayModelType != null) {
                        // add type imports
                        final ElmImport elmImport = createImport(cm.arrayModelType);
                        elmImports.add(elmImport);
                    }
                }
                if (cm.discriminator != null) {
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

                        child.vendorExtensions.put(DISCRIMINATOR_NAME, propertyName);
                    }
                }
                inner.put("elmImports", elmImports);
            }
        }
        return objs;
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

    @Override
    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> allModels) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>) objs.get("operation");

        boolean hasDateTime = false;
        boolean hasDate = false;
        final Map<String, Set<String>> dependencies = new HashMap<>();

        for (CodegenOperation op : ops) {
            String path = op.path;
            for (CodegenParameter param : op.pathParams) {
                final String var = paramToString(param);
                path = path.replace("{" + param.paramName + "}", "\" ++ " + var + " ++ \"");
                hasDateTime = hasDateTime || param.isDateTime;
                hasDate = hasDate || param.isDate;
            }
            op.path = ("\"" + path + "\"").replaceAll(" \\+\\+ \"\"", "");

            if (op.bodyParam != null && !op.bodyParam.isPrimitiveType && !op.bodyParam.isMapContainer) {
                final String encoder = (String) op.bodyParam.vendorExtensions.get(ENCODER);
                if (encoder != null) {
                    if (!dependencies.containsKey(op.bodyParam.dataType)) {
                        dependencies.put(op.bodyParam.dataType, new TreeSet<String>());
                    }
                }
            }
            for (CodegenResponse resp : op.responses) {
                if (resp.primitiveType || resp.isMapContainer) {
                    continue;
                }
                final String decoder = (String) resp.vendorExtensions.get(DECODER);
                if (decoder != null) {
                    if (!dependencies.containsKey(resp.dataType)) {
                        dependencies.put(resp.dataType, new TreeSet<String>());
                    }
                }
            }
        }

        final List<ElmImport> elmImports = new ArrayList<>();
        for (Map.Entry<String, Set<String>> entry : dependencies.entrySet()) {
            final ElmImport elmImport = new ElmImport();
            final String key = entry.getKey();
            elmImport.moduleName = "Data." + key;
            elmImport.as = key;
            elmImport.exposures = entry.getValue();
            elmImport.exposures.add(key);
            elmImport.hasExposures = true;
            elmImports.add(elmImport);
        }
        if (hasDate) {
            final ElmImport elmImport = new ElmImport();
            elmImport.moduleName = "DateOnly";
            elmImport.exposures = new TreeSet<>();
            elmImport.exposures.add("DateOnly");
            elmImport.hasExposures = true;
            elmImports.add(elmImport);
        }
        if (hasDateTime) {
            final ElmImport elmImport = new ElmImport();
            elmImport.moduleName = "DateTime";
            elmImport.exposures = new TreeSet<>();
            elmImport.exposures.add("DateTime");
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
                return toOptionalValue("\"" + p.getDefault().toString() + "\"");
            }
            return toOptionalValue(null);
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return toOptionalValue(Boolean.valueOf(p.getDefault().toString()) ? "True" : "False");
            }
            return toOptionalValue(null);
        } else if (ModelUtils.isDateSchema(p)) {
            return toOptionalValue(null);
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return toOptionalValue(null);
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return toOptionalValue(p.getDefault().toString());
            }
            return toOptionalValue(null);
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return toOptionalValue(p.getDefault().toString());
            }
            return toOptionalValue(null);
        } else {
            return toOptionalValue(null);
        }
    }

    private String toOptionalValue(String value) {
        if (value == null) {
            return "Nothing";
        }
        return "(Just " + value + ")";
    }

    private String paramToString(final CodegenParameter param) {
        final String paramName = param.paramName;

        if (param.isString || param.isUuid || param.isBinary || param.isByteArray) {
            return paramName;
        } else if (param.isBoolean) {
            return "if " + paramName + " then \"true\" else \"false\"";
        } else if (param.isDateTime) {
            return "DateTime.toString " + paramName;
        } else if (param.isDate) {
            return "DateOnly.toString " + paramName;
        } else if (ElmVersion.ELM_018.equals(elmVersion)) {
            return "toString " + paramName;
        } else if (param.isInteger || param.isLong) {
            return "String.fromInt " + paramName;
        } else if (param.isFloat || param.isDouble) {
            return "String.fromFloat " + paramName;
        }

        throw new RuntimeException("Parameter '" + paramName + "' cannot be converted to a string. Please report the issue.");
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
            property.vendorExtensions.put(UNION_TYPE, property.datatypeWithEnum);
        } else {
            final boolean isPrimitiveType = property.isMapContainer ? isPrimitiveDataType(property.dataType) : property.isPrimitiveType;
            addEncoderAndDecoder(property.vendorExtensions, property.dataType, isPrimitiveType ? DataTypeExposure.PRIMITIVE : DataTypeExposure.EXTERNAL);
        }

        return property;
    }

    @Override
    public CodegenResponse fromResponse(OpenAPI openAPI, String responseCode, ApiResponse resp) {
        final CodegenResponse response = super.fromResponse(openAPI, responseCode, resp);
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
            enumVar.put("name", prefix + enumVar.get("name"));
        }
    }

    private boolean isPrimitiveDataType(String dataType) {
        return languageSpecificPrimitives.contains(dataType);
    }

    private void addEncoderAndDecoder(final Map<String, Object> vendorExtensions, final String dataType, final DataTypeExposure dataTypeExposure) {
        final String baseName = org.openapitools.codegen.utils.StringUtils.camelize(dataType, true);
        String encoderName;
        String decoderName;
        switch (dataTypeExposure) {
            case EXPOSED:
                decoderName = "decoder";
                encoderName = "encoder";
                break;
            case INTERNAL:
                encoderName = baseName + "Encoder";
                decoderName = baseName + "Decoder";
                break;
            case EXTERNAL:
                encoderName = dataType + ".encoder";
                decoderName = dataType + ".decoder";
                break;
            case PRIMITIVE:
                encoderName = "Encode." + baseName;
                decoderName = "Decode." + baseName;
                break;
            default:
                encoderName = "";
                decoderName = "";
        }
        if (!vendorExtensions.containsKey(ENCODER)) {
            vendorExtensions.put(ENCODER, encoderName);
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
        }
    }
}
