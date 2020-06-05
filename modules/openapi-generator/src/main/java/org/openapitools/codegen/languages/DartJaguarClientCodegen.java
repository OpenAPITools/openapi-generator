/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;

import io.swagger.v3.oas.models.media.*;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.OnceLogger.once;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class DartJaguarClientCodegen extends DartClientCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(DartJaguarClientCodegen.class);
    private static final String NULLABLE_FIELDS = "nullableFields";
    private static final String SERIALIZATION_FORMAT = "serialization";
    private static final String IS_FORMAT_JSON = "jsonFormat";
    private static final String IS_FORMAT_PROTO = "protoFormat";
    private static final String CLIENT_NAME = "clientName";

    private static Set<String> modelToIgnore = new HashSet<>();
    private HashMap<String, String> protoTypeMapping = new HashMap<>();

    static {
        modelToIgnore.add("datetime");
        modelToIgnore.add("map");
        modelToIgnore.add("object");
        modelToIgnore.add("list");
        modelToIgnore.add("file");
        modelToIgnore.add("list<int>");
    }

    private static final String SERIALIZATION_JSON = "json";
    private static final String SERIALIZATION_PROTO = "proto";

    private boolean nullableFields = true;
    private String serialization = SERIALIZATION_JSON;

    public DartJaguarClientCodegen() {
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

        browserClient = false;
        outputFolder = "generated-code/dart-jaguar";
        embeddedTemplateDir = templateDir = "dart-jaguar";

        cliOptions.add(new CliOption(NULLABLE_FIELDS, "Is the null fields should be in the JSON payload"));
        cliOptions.add(new CliOption(SERIALIZATION_FORMAT, "Choose serialization format JSON or PROTO is supported"));

        typeMapping.put("file", "List<int>");
        typeMapping.put("binary", "List<int>");

        protoTypeMapping.put("Array", "repeated");
        protoTypeMapping.put("array", "repeated");
        protoTypeMapping.put("List", "repeated");
        protoTypeMapping.put("boolean", "bool");
        protoTypeMapping.put("string", "string");
        protoTypeMapping.put("char", "string");
        protoTypeMapping.put("int", "int32");
        protoTypeMapping.put("long", "int64");
        protoTypeMapping.put("short", "int32");
        protoTypeMapping.put("number", "double");
        protoTypeMapping.put("float", "float");
        protoTypeMapping.put("double", "double");
        protoTypeMapping.put("object", "google.protobuf.Any");
        protoTypeMapping.put("integer", "int32");
        protoTypeMapping.put("Date", "google.protobuf.Timestamp");
        protoTypeMapping.put("date", "google.protobuf.Timestamp");
        protoTypeMapping.put("File", "bytes");
        protoTypeMapping.put("file", "bytes");
        protoTypeMapping.put("binary", "bytes");
        protoTypeMapping.put("UUID", "string");
        protoTypeMapping.put("URI", "string");
        protoTypeMapping.put("ByteArray", "bytes");

    }

    @Override
    public String getName() {
        return "dart-jaguar";
    }

    @Override
    public String getHelp() {
        return "Generates a Dart Jaguar client library.";
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            return "const {}";
        } else if (ModelUtils.isArraySchema(p)) {
            return "const []";
        }
        return super.toDefaultValue(p);
    }

    @Override
    public void processOpts() {
        defaultProcessOpts();
        if (additionalProperties.containsKey(NULLABLE_FIELDS)) {
            nullableFields = convertPropertyToBooleanAndWriteBack(NULLABLE_FIELDS);
        } else {
            //not set, use to be passed to template
            additionalProperties.put(NULLABLE_FIELDS, nullableFields);
        }

        if (additionalProperties.containsKey(SERIALIZATION_FORMAT)) {
            serialization = ((String) additionalProperties.get(SERIALIZATION_FORMAT));
            boolean isProto = serialization.equalsIgnoreCase(SERIALIZATION_PROTO);
            additionalProperties.put(IS_FORMAT_JSON, serialization.equalsIgnoreCase(SERIALIZATION_JSON));
            additionalProperties.put(IS_FORMAT_PROTO, isProto);

            modelTemplateFiles.put("model.mustache", isProto ? ".proto" : ".dart");

        } else {
            //not set, use to be passed to template
            additionalProperties.put(IS_FORMAT_JSON, true);
            additionalProperties.put(IS_FORMAT_PROTO, false);
        }

        if (additionalProperties.containsKey(PUB_NAME)) {
            this.setPubName((String) additionalProperties.get(PUB_NAME));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(PUB_NAME, pubName);
        }
        additionalProperties.put(CLIENT_NAME, org.openapitools.codegen.utils.StringUtils.camelize(pubName));

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

        final String libFolder = sourceFolder + File.separator + "lib";
        supportingFiles.add(new SupportingFile("pubspec.mustache", "", "pubspec.yaml"));
        supportingFiles.add(new SupportingFile("analysis_options.mustache", "", "analysis_options.yaml"));
        supportingFiles.add(new SupportingFile("apilib.mustache", libFolder, "api.dart"));

        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));

        final String authFolder = sourceFolder + File.separator + "lib" + File.separator + "auth";
        supportingFiles.add(new SupportingFile("auth/api_key_auth.mustache", authFolder, "api_key_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/basic_auth.mustache", authFolder, "basic_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/oauth.mustache", authFolder, "oauth.dart"));
        supportingFiles.add(new SupportingFile("auth/auth.mustache", authFolder, "auth.dart"));
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = super.postProcessModels(objs);
        List<Object> models = (List<Object>) objs.get("models");
        ProcessUtils.addIndexToProperties(models, 1);

        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            Set<String> modelImports = new HashSet<>();
            CodegenModel cm = (CodegenModel) mo.get("model");
            for (String modelImport : cm.imports) {
                if (!modelToIgnore.contains(modelImport.toLowerCase(Locale.ROOT))) {
                    modelImports.add(underscore(modelImport));
                }
            }

            for (CodegenProperty p : cm.vars) {
                String protoType = protoTypeMapping.get(p.openApiType);
                if (p.isListContainer) {
                    String innerType = protoTypeMapping.get(p.mostInnerItems.openApiType);
                    protoType = protoType + " " + (innerType == null ? p.mostInnerItems.openApiType : innerType);
                }
                p.vendorExtensions.put("x-proto-type", protoType == null ? p.openApiType : protoType);
            }

            cm.imports = modelImports;
            boolean hasVars = cm.vars.size() > 0;
            cm.vendorExtensions.put("x-has-vars", hasVars);
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        Set<String> modelImports = new HashSet<>();
        Set<String> fullImports = new HashSet<>();
        for (CodegenOperation op : operationList) {
            op.httpMethod = StringUtils.capitalize(op.httpMethod.toLowerCase(Locale.ROOT));
            boolean isJson = true; //default to JSON
            boolean isForm = false;
            boolean isProto = false;
            boolean isMultipart = false;
            if (op.consumes != null) {
                for (Map<String, String> consume : op.consumes) {
                    if (consume.containsKey("mediaType")) {
                        String type = consume.get("mediaType");
                        isJson = type.equalsIgnoreCase("application/json");
                        isProto = type.equalsIgnoreCase("application/octet-stream");
                        isForm = type.equalsIgnoreCase("application/x-www-form-urlencoded");
                        isMultipart = type.equalsIgnoreCase("multipart/form-data");
                        break;
                    }
                }
            }

            for (CodegenParameter param : op.allParams) {
                if (param.baseType != null && param.baseType.equalsIgnoreCase("List<int>") && isMultipart) {
                    param.baseType = "MultipartFile";
                    param.dataType = "MultipartFile";
                }
            }
            for (CodegenParameter param : op.formParams) {
                if (param.baseType != null && param.baseType.equalsIgnoreCase("List<int>") && isMultipart) {
                    param.baseType = "MultipartFile";
                    param.dataType = "MultipartFile";
                }
            }
            for (CodegenParameter param : op.bodyParams) {
                if (param.baseType != null && param.baseType.equalsIgnoreCase("List<int>") && isMultipart) {
                    param.baseType = "MultipartFile";
                    param.dataType = "MultipartFile";
                }
            }

            op.vendorExtensions.put("x-is-form", isForm);
            op.vendorExtensions.put("x-is-json", isJson);
            op.vendorExtensions.put("x-is-proto", isProto);
            op.vendorExtensions.put("x-is-multipart", isMultipart);


            Set<String> imports = new HashSet<>();
            for (String item : op.imports) {
                if (!modelToIgnore.contains(item.toLowerCase(Locale.ROOT))) {
                    imports.add(underscore(item));
                }
            }
            modelImports.addAll(imports);
            op.imports = imports;

            String[] items = op.path.split("/", -1);
            String jaguarPath = "";

            for (int i = 0; i < items.length; ++i) {
                if (items[i].matches("^\\{(.*)\\}$")) { // wrap in {}
                    jaguarPath = jaguarPath + ":" + items[i].replace("{", "").replace("}", "");
                } else {
                    jaguarPath = jaguarPath + items[i];
                }

                if (i != items.length - 1) {
                    jaguarPath = jaguarPath + "/";
                }
            }

            op.path = jaguarPath;
        }

        objs.put("modelImports", modelImports);
        objs.put("fullImports", fullImports);

        return objs;
    }
}
