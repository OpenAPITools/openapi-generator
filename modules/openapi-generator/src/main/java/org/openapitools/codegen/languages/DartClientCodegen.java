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

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.ClientModificationFeature;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;

import static org.openapitools.codegen.utils.StringUtils.*;

public class DartClientCodegen extends DefaultCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(DartClientCodegen.class);

    public static final String PUB_LIBRARY = "pubLibrary";
    public static final String PUB_NAME = "pubName";
    public static final String PUB_VERSION = "pubVersion";
    public static final String PUB_DESCRIPTION = "pubDescription";
    public static final String PUB_AUTHOR = "pubAuthor";
    public static final String PUB_AUTHOR_EMAIL = "pubAuthorEmail";
    public static final String PUB_HOMEPAGE = "pubHomepage";
    public static final String USE_ENUM_EXTENSION = "useEnumExtension";

    protected String pubLibrary = "openapi.api";
    protected String pubName = "openapi";
    protected String pubVersion = "1.0.0";
    protected String pubDescription = "OpenAPI API client";
    protected String pubAuthor = "Author";
    protected String pubAuthorEmail = "author@homepage";
    protected String pubHomepage = "homepage";
    protected boolean useEnumExtension = false;
    protected String sourceFolder = "";
    protected String apiDocPath = "doc" + File.separator;
    protected String modelDocPath = "doc" + File.separator;
    protected String apiTestPath = "test" + File.separator;
    protected String modelTestPath = "test" + File.separator;

    public DartClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.of(
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath
                )
        );

        // clear import mapping (from default generator) as dart does not use it at the moment
        importMapping.clear();

        outputFolder = "generated-code/dart";
        modelTemplateFiles.put("model.mustache", ".dart");
        apiTemplateFiles.put("api.mustache", ".dart");
        embeddedTemplateDir = templateDir = "dart2";
        apiPackage = "lib.api";
        modelPackage = "lib.model";
        modelDocTemplateFiles.put("object_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        modelTestTemplateFiles.put("model_test.mustache", ".dart");
        apiTestTemplateFiles.put("api_test.mustache", ".dart");

        List<String> reservedWordsList = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(
                    new InputStreamReader(DartClientCodegen.class.getResourceAsStream("/dart/dart-keywords.txt"),
                            StandardCharsets.UTF_8));
            while (reader.ready()) {
                reservedWordsList.add(reader.readLine());
            }
            reader.close();
        } catch (Exception e) {
            LOGGER.error("Error reading dart keywords. Exception: {}", e.getMessage());
        }
        setReservedWordsLowerCase(reservedWordsList);

        languageSpecificPrimitives = Sets.newHashSet(
            "String",
            "bool",
            "int",
            "num",
            "double"
        );
        instantiationTypes.put("array", "List");
        instantiationTypes.put("map", "Map");

        typeMapping = new HashMap<>();
        typeMapping.put("Array", "List");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("List", "List");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "String");
        typeMapping.put("char", "String");
        typeMapping.put("int", "int");
        typeMapping.put("long", "int");
        typeMapping.put("short", "int");
        typeMapping.put("number", "num");
        typeMapping.put("float", "double");
        typeMapping.put("double", "double");
        typeMapping.put("decimal", "double");
        typeMapping.put("integer", "int");
        typeMapping.put("Date", "DateTime");
        typeMapping.put("date", "DateTime");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("File", "MultipartFile");
        typeMapping.put("binary", "MultipartFile");
        typeMapping.put("UUID", "String");
        typeMapping.put("URI", "String");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("object", "Object");
        typeMapping.put("AnyType", "Object");

        // These are needed as they prevent models from being generated
        // which would clash with existing types, e.g. List
        // Importing dart:core doesn't hurt but a subclass may choose to skip
        // dart:core imports.
        importMapping.put("dynamic", "dart:core");
        importMapping.put("Object", "dart:core");
        importMapping.put("String", "dart:core");
        importMapping.put("bool", "dart:core");
        importMapping.put("num", "dart:core");
        importMapping.put("int", "dart:core");
        importMapping.put("double", "dart:core");
        importMapping.put("List", "dart:core");
        importMapping.put("Map", "dart:core");
        importMapping.put("Set", "dart:core");
        importMapping.put("DateTime", "dart:core");

        cliOptions.add(new CliOption(PUB_LIBRARY, "Library name in generated code"));
        cliOptions.add(new CliOption(PUB_NAME, "Name in generated pubspec"));
        cliOptions.add(new CliOption(PUB_VERSION, "Version in generated pubspec"));
        cliOptions.add(new CliOption(PUB_DESCRIPTION, "Description in generated pubspec"));
        cliOptions.add(new CliOption(PUB_AUTHOR, "Author name in generated pubspec"));
        cliOptions.add(new CliOption(PUB_AUTHOR_EMAIL, "Email address of the author in generated pubspec"));
        cliOptions.add(new CliOption(PUB_HOMEPAGE, "Homepage in generated pubspec"));
        cliOptions.add(new CliOption(USE_ENUM_EXTENSION, "Allow the 'x-enum-values' extension for enums"));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, "Source folder for generated code"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "dart";
    }

    @Override
    public String getHelp() {
        return "Generates a Dart 2.x client library.";
    }

    protected void defaultProcessOpts() {
        super.processOpts();
    }

    @Override
    public void processOpts() {
        defaultProcessOpts();

        if (StringUtils.isEmpty(System.getenv("DART_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable DART_POST_PROCESS_FILE not defined so the Dart code may not be properly formatted. To define it, try `export DART_POST_PROCESS_FILE=\"/usr/local/bin/dartfmt -w\"` (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(PUB_NAME)) {
            this.setPubName((String) additionalProperties.get(PUB_NAME));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(PUB_NAME, pubName);
        }

        if (additionalProperties.containsKey(PUB_LIBRARY)) {
            this.setPubLibrary((String) additionalProperties.get(PUB_LIBRARY));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(PUB_LIBRARY, pubLibrary);
        }

        if (additionalProperties.containsKey(PUB_VERSION)) {
            this.setPubVersion((String) additionalProperties.get(PUB_VERSION));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(PUB_VERSION, pubVersion);
        }

        if (additionalProperties.containsKey(PUB_DESCRIPTION)) {
            this.setPubDescription((String) additionalProperties.get(PUB_DESCRIPTION));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(PUB_DESCRIPTION, pubDescription);
        }

        if (additionalProperties.containsKey(PUB_AUTHOR)) {
            this.setPubAuthor((String) additionalProperties.get(PUB_AUTHOR));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(PUB_AUTHOR, pubAuthor);
        }

        if (additionalProperties.containsKey(PUB_AUTHOR_EMAIL)) {
            this.setPubAuthorEmail((String) additionalProperties.get(PUB_AUTHOR_EMAIL));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(PUB_AUTHOR_EMAIL, pubAuthorEmail);
        }

        if (additionalProperties.containsKey(PUB_HOMEPAGE)) {
            this.setPubHomepage((String) additionalProperties.get(PUB_HOMEPAGE));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(PUB_HOMEPAGE, pubHomepage);
        }

        if (additionalProperties.containsKey(USE_ENUM_EXTENSION)) {
            this.setUseEnumExtension(convertPropertyToBooleanAndWriteBack(USE_ENUM_EXTENSION));
        } else {
            // Not set, use to be passed to template.
            additionalProperties.put(USE_ENUM_EXTENSION, useEnumExtension);
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        // check to not overwrite a custom templateDir
        if (templateDir == null) {
            embeddedTemplateDir = templateDir = "dart2";
        }

        final String libFolder = sourceFolder + File.separator + "lib";
        supportingFiles.add(new SupportingFile("pubspec.mustache", "", "pubspec.yaml"));
        supportingFiles.add(new SupportingFile("api_client.mustache", libFolder, "api_client.dart"));
        supportingFiles.add(new SupportingFile("api_exception.mustache", libFolder, "api_exception.dart"));
        supportingFiles.add(new SupportingFile("api_helper.mustache", libFolder, "api_helper.dart"));
        supportingFiles.add(new SupportingFile("apilib.mustache", libFolder, "api.dart"));

        final String authFolder = sourceFolder + File.separator + "lib" + File.separator + "auth";
        supportingFiles.add(new SupportingFile("auth/authentication.mustache", authFolder, "authentication.dart"));
        supportingFiles.add(new SupportingFile("auth/http_basic_auth.mustache", authFolder, "http_basic_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/http_bearer_auth.mustache", authFolder, "http_bearer_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/api_key_auth.mustache", authFolder, "api_key_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/oauth.mustache", authFolder, "oauth.dart"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
    }

    @Override
    public String escapeReservedWord(String name) {
        return name + "_";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + apiTestPath.replace('/', File.separatorChar);
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + modelTestPath.replace('/', File.separatorChar);
    }

    @Override
    public String apiDocFileFolder() {
        return outputFolder + File.separator + apiDocPath.replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return outputFolder + File.separator + modelDocPath.replace('/', File.separatorChar);
    }

    @Override
    public String toVarName(String name) {
        return  toVarName(name, "n");
    }

    private String toVarName(String name, String numberPrefix) {
        // replace - with _ e.g. created-at => created_at
        name = name.replace("-", "_");

        // always need to replace leading underscores first
        name = name.replaceAll("^_", "");

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // replace all characters that have a mapping but ignore underscores
        // append an underscore to each replacement so that it can be camelized
        if (name.chars().anyMatch(character -> specialCharReplacements.containsKey("" + ((char) character)))) {
            name = escape(name, specialCharReplacements, Lists.newArrayList("_"), "_");
        }
        // remove the rest
        name = sanitizeName(name);

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, true);

        if (name.matches("^\\d.*")) {
            name = numberPrefix + name;
        }

        if (isReservedWord(name) || importMapping().containsKey(name)) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(final String name) {
        if (importMapping().containsKey(name)) {
            return name;
        }

        String nameWithPrefixSuffix = sanitizeName(name);
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            // add '_' so that model name can be camelized correctly
            nameWithPrefixSuffix = modelNamePrefix + "_" + nameWithPrefixSuffix;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            // add '_' so that model name can be camelized correctly
            nameWithPrefixSuffix = nameWithPrefixSuffix + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        final String camelizedName = camelize(nameWithPrefixSuffix);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            final String modelName = "Model" + camelizedName;
            LOGGER.warn(camelizedName + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (camelizedName.matches("^\\d.*")) {
            final String modelName = "Model" + camelizedName; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        return camelizedName;
    }

    @Override
    public String toModelFilename(String name) {
        return underscore(toModelName(name));
    }

    @Override public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(String name) {
        return underscore(toApiName(name));
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiFilename(name) + "_test";
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelFilename(name) + "_test";
    }

    @Override
    public String toDefaultValue(Schema schema) {
        if (ModelUtils.isMapSchema(schema)) {
            return "const {}";
        } else if (ModelUtils.isArraySchema(schema)) {
            return "const []";
        }

        if (schema.getDefault() != null) {
            if (ModelUtils.isStringSchema(schema)) {
                return "'" + schema.getDefault().toString().replace("'", "\\'") + "'";
            }
            return schema.getDefault().toString();
        } else {
            return null;
        }
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return instantiationTypes().get("array") + "<" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return instantiationTypes().get("map") + "<String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String type = super.getSchemaType(p);
        if (typeMapping.containsKey(type)) {
            return typeMapping.get(type);
        }
        if (languageSpecificPrimitives.contains(type)) {
            return type;
        }
        return toModelName(type);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(objs);
    }

    @Override
    protected void updateEnumVarsWithExtensions(List<Map<String, Object>> enumVars, Map<String, Object> vendorExtensions, String dataType) {
        if (vendorExtensions != null && useEnumExtension && vendorExtensions.containsKey("x-enum-values")) {
            // Use the x-enum-values extension for this enum
            // Existing enumVars added by the default handling need to be removed first
            enumVars.clear();

            Object extension = vendorExtensions.get("x-enum-values");
            List<Map<String, Object>> values = (List<Map<String, Object>>) extension;
            for (Map<String, Object> value : values) {
                Map<String, Object> enumVar = new HashMap<>();
                enumVar.put("name", toEnumVarName((String) value.get("identifier"), dataType));
                enumVar.put("value", toEnumValue(value.get("numericValue").toString(), dataType));
                enumVar.put("isString", isDataTypeString(dataType));
                if (value.containsKey("description")) {
                    enumVar.put("description", value.get("description").toString());
                }
                enumVars.add(enumVar);
            }
        } else {
            super.updateEnumVarsWithExtensions(enumVars, vendorExtensions, dataType);
        }
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (value.length() == 0) {
            return "empty";
        }
        return toVarName(value, "number");
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("number".equalsIgnoreCase(datatype) ||
                "int".equalsIgnoreCase(datatype)) {
            return value;
        } else {
            return "'" + escapeText(value) + "'";
        }
    }

    @Override
    public String toOperationId(String operationId) {
        operationId = super.toOperationId(operationId);

        operationId = camelize(sanitizeName(operationId), true);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize("call_" + operationId, true);
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn(operationId + " (starting with a number) cannot be used as method name. Renamed to " + camelize("call_" + operationId), true);
            operationId = camelize("call_" + operationId, true);
        }

        return operationId;
    }

    public void setPubLibrary(String pubLibrary) {
        this.pubLibrary = pubLibrary;
    }

    public void setPubName(String pubName) {
        this.pubName = pubName;
    }

    public void setPubVersion(String pubVersion) {
        this.pubVersion = pubVersion;
    }

    public void setPubDescription(String pubDescription) {
        this.pubDescription = pubDescription;
    }

    public void setPubAuthor(String pubAuthor) {
        this.pubAuthor = pubAuthor;
    }

    public void setPubAuthorEmail(String pubAuthorEmail) {
        this.pubAuthorEmail = pubAuthorEmail;
    }

    public void setPubHomepage(String pubHomepage) {
        this.pubHomepage = pubHomepage;
    }

    public void setUseEnumExtension(boolean useEnumExtension) {
        this.useEnumExtension = useEnumExtension;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
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

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        String dartPostProcessFile = System.getenv("DART_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(dartPostProcessFile)) {
            return; // skip if DART_POST_PROCESS_FILE env variable is not defined
        }

        // only process the following type (or we can simply rely on the file extension to check if it's a Dart file)
        Set<String> supportedFileType = Sets.newHashSet(
            "supporting-mustache",
            "model-test",
            "model",
            "api-test",
            "api");
        if (!supportedFileType.contains(fileType)) {
            return;
        }

        // only process files with dart extension
        if ("dart".equals(FilenameUtils.getExtension(file.toString()))) {
            // currently only support "dartfmt -w yourcode.dart"
            String command = dartPostProcessFile + " " + file.toString();
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit code: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (Exception e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
            }
        }
    }
}
