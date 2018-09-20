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

public class ElmClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ElmClientCodegen.class);
    private Set<String> customPrimitives = new HashSet<String>();
    private ElmVersion elmVersion = ElmVersion.ELM_019;

    private static final String ELM_VERSION = "elmVersion";
    private static final String ENCODER = "elmEncoder";
    private static final String DECODER = "elmDecoder";
    private static final String X_DISCRIMINATOR_TYPE = "x-discriminator-value";
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

        typeMapping.clear();
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Int");
        typeMapping.put("number", "Float");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Float");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "String");
        typeMapping.put("array", "List");
        typeMapping.put("date", "DateOnly");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("password", "String");
        typeMapping.put("file", "String");
        typeMapping.put("ByteArray", "Byte");
        typeMapping.put("binary", "String");

        importMapping.clear();

        cliOptions.clear();

        final CliOption elmVersion = new CliOption(ELM_VERSION, "Elm version: 0.18, 0.19").defaultValue("0.19");
        final Map<String, String> supportedVersions = new HashMap<>();
        supportedVersions.put("0.18", "Elm 0.18");
        supportedVersions.put("0.19", "Elm 0.19");
        elmVersion.setEnum(supportedVersions);
        cliOptions.add(elmVersion);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("ELM_FORMAT_PATH"))) {
            LOGGER.info("Environment variable ELM_FORMAT_PATH not defined so the Elm code may not be properly formatted. To define it, try 'export ELM_FORMAT_PATH=/usr/local/bin/elm-format' (Linux/Mac)");
        }

        if (additionalProperties.containsKey(ELM_VERSION)) {
            final String version = (String) additionalProperties.get(ELM_VERSION);
            if ("0.18".equals(version)) {
                elmVersion = ElmVersion.ELM_018;
            }
        }

        switch (elmVersion) {
            case ELM_018:
                additionalProperties.put("isElm018", true);
                break;
            case ELM_019:
                additionalProperties.put("isElm019", true);
                break;
            default:
                throw new RuntimeException("Undefined Elm version");
        }

        supportingFiles.add(new SupportingFile("Byte.mustache", "src", "Byte.elm"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        if (ElmVersion.ELM_018.equals(elmVersion)) {
            supportingFiles.add(new SupportingFile("DateOnly018.mustache", "src", "DateOnly.elm"));
            supportingFiles.add(new SupportingFile("DateTime018.mustache", "src", "DateTime.elm"));
            supportingFiles.add(new SupportingFile("elm-package.mustache", "", "elm-package.json"));
            supportingFiles.add(new SupportingFile("Main018.mustache", "src", "Main.elm"));
        }
        if (ElmVersion.ELM_019.equals(elmVersion)) {
            supportingFiles.add(new SupportingFile("DateOnly019.mustache", "src", "DateOnly.elm"));
            supportingFiles.add(new SupportingFile("DateTime019.mustache", "src", "DateTime.elm"));
            supportingFiles.add(new SupportingFile("elm.mustache", "", "elm.json"));
            supportingFiles.add(new SupportingFile("Main019.mustache", "src", "Main.elm"));
        }
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
        final String camelized = org.openapitools.codegen.utils.StringUtils.camelize(value.replace(" ", "_").replace("(", "_").replace(")", "")); // TODO FIXME escape properly
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
                    this.addEncoderAndDecoder(cm.vendorExtensions, cm.classname, false, false);
                    cm.vendorExtensions.put(UNION_TYPE, cm.classname);
                } else if (cm.isAlias) {
                    this.addEncoderAndDecoder(cm.vendorExtensions, cm.dataType, false, true);
                }

                List<ElmImport> elmImports = new ArrayList<>();
                for (CodegenProperty property : cm.allVars) {
                    if (property.complexType != null) {
                        elmImports.add(createPropertyImport(property));
                    }
                }
                if (cm.isArrayModel) {
                    if (cm.arrayModelType != null) {
                        // add type imports
                        final ElmImport elmImport = new ElmImport();
                        final String modulePrefix = customPrimitives.contains(cm.arrayModelType) ? "" : "Data.";
                        elmImport.moduleName = modulePrefix + cm.arrayModelType;
                        elmImport.exposures = new TreeSet<>();
                        elmImport.exposures.add(cm.arrayModelType);
                        elmImport.exposures.add(org.openapitools.codegen.utils.StringUtils.camelize(cm.arrayModelType, true) + "Decoder");
                        elmImport.exposures.add(org.openapitools.codegen.utils.StringUtils.camelize(cm.arrayModelType, true) + "Encoder");
                        elmImport.hasExposures = true;
                        elmImports.add(elmImport);
                    }
                }
                if (cm.discriminator != null) {
                    for (CodegenModel child : cm.children) {
                        // add child imports
                        final ElmImport elmImport = new ElmImport();
                        final String modulePrefix = customPrimitives.contains(child.classname) ? "" : "Data.";
                        elmImport.moduleName = modulePrefix + child.classname;
                        elmImport.exposures = new TreeSet<>();
                        elmImport.exposures.add(child.classname);
                        elmImport.exposures.add(child.classVarName + "Decoder");
                        elmImport.exposures.add(child.classVarName + "Encoder");
                        elmImport.hasExposures = true;
                        elmImports.add(elmImport);

                        // set discriminator value to all children (recursively)
                        this.setDiscriminatorValue(child, cm.getDiscriminatorName(), this.getDiscriminatorValue(child));

                        // add all non-discriminator vars
                        int index = 0;
                        for (CodegenProperty property : cm.vars) {
                            if (!cm.discriminator.equals(property.baseName)) {
                                child.vars.add(index++, property);
                            }
                        }
                    }
                }
                inner.put("elmImports", elmImports);
            }
        }
        return objs;
    }

    private void setDiscriminatorValue(CodegenModel model, String baseName, String value) {
        for (CodegenProperty prop : model.vars) {
            if (prop.baseName.equals(baseName)) {
                prop.discriminatorValue = value;
            }
        }
        for (CodegenProperty prop : model.allVars) {
            if (prop.baseName.equals(baseName)) {
                prop.discriminatorValue = value;
            }
        }
        if (model.children != null) {
            final boolean newDiscriminator = model.discriminator != null;
            for (CodegenModel child : model.children) {
                this.setDiscriminatorValue(child, baseName, newDiscriminator ? value : this.getDiscriminatorValue(child));
            }
        }
    }

    private String getDiscriminatorValue(CodegenModel model) {
        return model.vendorExtensions.containsKey(X_DISCRIMINATOR_TYPE) ?
                (String) model.vendorExtensions.get(X_DISCRIMINATOR_TYPE) : model.classname;
    }

    private ElmImport createPropertyImport(final CodegenProperty property) {
        final ElmImport elmImport = new ElmImport();
        final String modulePrefix = customPrimitives.contains(property.complexType) ? "" : "Data.";
        elmImport.moduleName = modulePrefix + property.complexType;
        elmImport.exposures = new TreeSet<>();
        elmImport.exposures.add(property.complexType);
        if (property.vendorExtensions.containsKey(DECODER)) {
            elmImport.exposures.add((String) property.vendorExtensions.get(DECODER));
        }
        if (property.vendorExtensions.containsKey(ENCODER)) {
            elmImport.exposures.add((String) property.vendorExtensions.get(ENCODER));
        }
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

        Map<String, Set<String>> dependencies = new HashMap<>();

        for (CodegenOperation op : ops) {
            String path = op.path;
            for (CodegenParameter param : op.pathParams) {
                final String var = paramToString(param);
                path = path.replace("{" + param.paramName + "}", "\" ++ " + var + " ++ \"");
            }
            op.path = ("\"" + path + "\"").replaceAll(" \\+\\+ \"\"", "");

            if (op.bodyParam != null && !op.bodyParam.isPrimitiveType && !op.bodyParam.isMapContainer) {
                final String encoder = (String) op.bodyParam.vendorExtensions.get(ENCODER);
                if (encoder != null) {
                    if (!dependencies.containsKey(op.bodyParam.dataType)) {
                        dependencies.put(op.bodyParam.dataType, new TreeSet<String>());
                    }
                    dependencies.get(op.bodyParam.dataType).add(encoder);
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
                    dependencies.get(resp.dataType).add(decoder);
                }
            }
        }

        List<ElmImport> elmImports = new ArrayList<>();
        for (Map.Entry<String, Set<String>> entry : dependencies.entrySet()) {
            final ElmImport elmImport = new ElmImport();
            final String key = entry.getKey();
            elmImport.moduleName = "Data." + key;
            elmImport.exposures = entry.getValue();
            elmImport.exposures.add(key);
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
            NumberSchema dp = (NumberSchema) p;
            if (dp.getDefault() != null) {
                return toOptionalValue(dp.getDefault().toString());
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
        if (param.isString) {
            return paramName;
        }
        if (ElmVersion.ELM_018.equals(elmVersion)) {
            return "toString " + paramName;
        }
        if (param.isInteger || param.isLong) {
            return "String.fromInt " + paramName;
        }
        if (param.isFloat || param.isDouble) {
            return "String.fromFloat " + paramName;
        }
        throw new RuntimeException("Parameter '" + paramName + "' cannot be converted to a string");
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

        final String dataType = property.isEnum ? property.baseName : property.dataType;
        addEncoderAndDecoder(property.vendorExtensions, dataType, property.isMapContainer, property.isPrimitiveType && !property.isEnum);
        if (property.isEnum) {
            property.vendorExtensions.put(UNION_TYPE, property.datatypeWithEnum);
        }

        return property;
    }

    @Override
    public CodegenResponse fromResponse(OpenAPI openAPI, String responseCode, ApiResponse resp) {
        final CodegenResponse response = super.fromResponse(openAPI, responseCode, resp);
        if (response.dataType != null) {
            addEncoderAndDecoder(response.vendorExtensions, response.dataType, response.isMapContainer, response.primitiveType);
        }
        return response;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        addEncoderAndDecoder(parameter.vendorExtensions, parameter.dataType, parameter.isMapContainer, parameter.isPrimitiveType);
    }

    private boolean isPrimitiveDataType(String dataType) {
        return languageSpecificPrimitives.contains(dataType);
    }

    private void addEncoderAndDecoder(Map<String, Object> vendorExtensions, String dataType, Boolean isMapContainer, Boolean isPrimitiveType) {
        if (isMapContainer) {
            isPrimitiveType = isPrimitiveDataType(dataType);
        }
        final String baseName = org.openapitools.codegen.utils.StringUtils.camelize(dataType, true);
        String encoderName;
        String decoderName;
        if (isPrimitiveType) {
            encoderName = "Encode." + baseName;
            decoderName = "Decode." + baseName;
        } else {
            encoderName = baseName + "Encoder";
            decoderName = baseName + "Decoder";
        }
        if (!vendorExtensions.containsKey(ENCODER)) {
            vendorExtensions.put(ENCODER, encoderName);
        }
        if (!vendorExtensions.containsKey(DECODER)) {
            vendorExtensions.put(DECODER, decoderName);
        }
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

        String elmFmtPath = System.getenv("ELM_FORMAT_PATH");
        if (StringUtils.isEmpty(elmFmtPath)) {
            return; // skip if ELM_FORMAT_PATH env variable is not defined
        }

        // only process files with elm extension
        if ("elm".equals(FilenameUtils.getExtension(file.toString()))) {
            // currently only support "elm-format -w yourcode.elm"
            String command = elmFmtPath + " --yes " + file.toString();
            if (ElmVersion.ELM_018.equals(elmVersion)) {
                command += " --elm-version=0.18";
            }

            try {
                Process p = Runtime.getRuntime().exec(command);
                p.waitFor();
                if (p.exitValue() != 0) {
                    LOGGER.error("Error running the command ({}). Exit code: {}", command, p.exitValue());
                }
                LOGGER.info("Successfully executed: " + command);
            } catch (Exception e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
            }
        }
    }
}
