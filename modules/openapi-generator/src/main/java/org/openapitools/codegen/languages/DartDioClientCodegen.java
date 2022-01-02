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

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Sets;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.ClientModificationFeature;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.underscore;

public class DartDioClientCodegen extends AbstractDartCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(DartDioClientCodegen.class);

    public static final String NULLABLE_FIELDS = "nullableFields";
    public static final String DATE_LIBRARY = "dateLibrary";

    private static final String CLIENT_NAME = "clientName";

    private boolean nullableFields = false;
    private String dateLibrary = "core";

    public DartDioClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeClientModificationFeatures(
                        ClientModificationFeature.Authorizations,
                        ClientModificationFeature.UserAgent
                )
        );

        outputFolder = "generated-code/dart-dio";
        embeddedTemplateDir = "dart-dio";
        this.setTemplateDir(embeddedTemplateDir);

        cliOptions.add(new CliOption(NULLABLE_FIELDS, "Make all fields nullable in the JSON payload"));
        CliOption dateLibrary = new CliOption(DATE_LIBRARY, "Option. Date library to use").defaultValue(this.getDateLibrary());
        Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put("core", "Dart core library (DateTime)");
        dateOptions.put("timemachine", "Time Machine is date and time library for Flutter, Web, and Server with support for timezones, calendars, cultures, formatting and parsing.");
        dateLibrary.setEnum(dateOptions);
        cliOptions.add(dateLibrary);

        typeMapping.put("Array", "BuiltList");
        typeMapping.put("array", "BuiltList");
        typeMapping.put("List", "BuiltList");
        typeMapping.put("set", "BuiltSet");
        typeMapping.put("map", "BuiltMap");
        typeMapping.put("file", "Uint8List");
        typeMapping.put("binary", "Uint8List");
        typeMapping.put("object", "JsonObject");
        typeMapping.put("AnyType", "JsonObject");

        imports.put("BuiltList", "package:built_collection/built_collection.dart");
        imports.put("BuiltSet", "package:built_collection/built_collection.dart");
        imports.put("BuiltMap", "package:built_collection/built_collection.dart");
        imports.put("JsonObject", "package:built_value/json_object.dart");
        imports.put("Uint8List", "dart:typed_data");
    }

    public String getDateLibrary() {
        return dateLibrary;
    }

    public void setDateLibrary(String library) {
        this.dateLibrary = library;
    }

    public boolean getNullableFields() {
        return nullableFields;
    }

    public void setNullableFields(boolean nullableFields) {
        this.nullableFields = nullableFields;
    }

    @Override
    public String getName() {
        return "dart-dio";
    }

    @Override
    public String getHelp() {
        return "Generates a Dart Dio client library.";
    }

    @Override
    protected ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("escapeBuiltValueEnum", (fragment, writer) -> {
                    // Raw strings don't work correctly in built_value enum strings.
                    // Dollar signs need to be escaped in to make them work.
                    // @BuiltValueEnumConst(wireName: r'$') produces '$' in generated code.
                    // @BuiltValueEnumConst(wireName: r'\$') produces '\$' in generated code.
                    writer.write(fragment.execute().replace("$", "\\$"));
                });
    }

    @Override
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            if (ModelUtils.isArraySchema(schema)) {
                if (ModelUtils.isSet(schema)) {
                    return "SetBuilder()";
                }
                return "ListBuilder()";
            }
            if (ModelUtils.isMapSchema(schema)) {
                return "MapBuilder()";
            }
            if (ModelUtils.isDateSchema(schema) || ModelUtils.isDateTimeSchema(schema)) {
                // this is currently not supported and would create compile errors
                return null;
            }
            if (ModelUtils.isStringSchema(schema)) {
                return "'" + schema.getDefault().toString().replaceAll("'", "\\'") + "'";
            }
            return schema.getDefault().toString();
        }
        return null;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("DART_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable DART_POST_PROCESS_FILE not defined so the Dart code may not be properly formatted. To define it, try `export DART_POST_PROCESS_FILE=\"/usr/local/bin/dartfmt -w\"` (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(NULLABLE_FIELDS)) {
            this.setNullableFields(convertPropertyToBooleanAndWriteBack(NULLABLE_FIELDS));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(NULLABLE_FIELDS, nullableFields);
        }

        if (!additionalProperties.containsKey(CLIENT_NAME)) {
            additionalProperties.put(CLIENT_NAME, org.openapitools.codegen.utils.StringUtils.camelize(pubName));
        }

        if (additionalProperties.containsKey(DATE_LIBRARY)) {
            this.setDateLibrary(additionalProperties.get(DATE_LIBRARY).toString());
        }
        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        final String libFolder = sourceFolder + File.separator + "lib";
        supportingFiles.add(new SupportingFile("pubspec.mustache", "", "pubspec.yaml"));
        supportingFiles.add(new SupportingFile("analysis_options.mustache", "", "analysis_options.yaml"));
        supportingFiles.add(new SupportingFile("apilib.mustache", libFolder, "api.dart"));
        supportingFiles.add(new SupportingFile("api_util.mustache", libFolder, "api_util.dart"));

        supportingFiles.add(new SupportingFile("serializers.mustache", libFolder, "serializers.dart"));

        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        final String authFolder = libFolder + File.separator + "auth";
        supportingFiles.add(new SupportingFile("auth/api_key_auth.mustache", authFolder, "api_key_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/basic_auth.mustache", authFolder, "basic_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/oauth.mustache", authFolder, "oauth.dart"));
        supportingFiles.add(new SupportingFile("auth/auth.mustache", authFolder, "auth.dart"));

        if ("core".equals(dateLibrary)) {
            // this option uses the same classes as normal dart generator
            additionalProperties.put("core", "true");
        } else if ("timemachine".equals(dateLibrary)) {
            additionalProperties.put("timeMachine", "true");
            typeMapping.put("date", "OffsetDate");
            typeMapping.put("Date", "OffsetDate");
            typeMapping.put("DateTime", "OffsetDateTime");
            typeMapping.put("datetime", "OffsetDateTime");
            imports.put("OffsetDate", "package:time_machine/time_machine.dart");
            imports.put("OffsetDateTime", "package:time_machine/time_machine.dart");
            supportingFiles.add(new SupportingFile("local_date_serializer.mustache", libFolder, "local_date_serializer.dart"));
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = super.postProcessModels(objs);
        List<Object> models = (List<Object>) objs.get("models");
        ProcessUtils.addIndexToProperties(models, 1);

        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = rewriteImports(cm.imports);
            cm.vendorExtensions.put("x-has-vars", !cm.vars.isEmpty());
        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (nullableFields) {
            property.isNullable = true;
        }

        if (property.isEnum) {
            // enums are generated with built_value and make use of BuiltSet
            model.imports.add("BuiltSet");
        }

        property.getVendorExtensions().put("x-built-value-serializer-type", createBuiltValueSerializerType(property));
    }

    private String createBuiltValueSerializerType(CodegenProperty property) {
        final StringBuilder sb = new StringBuilder("const FullType(");
        if (property.isContainer) {
            appendCollection(sb, property);
        } else {
            sb.append(property.datatypeWithEnum);
        }
        sb.append(")");
        return sb.toString();
    }

    private void appendCollection(StringBuilder sb, CodegenProperty property) {
        sb.append(property.baseType);
        sb.append(", [FullType(");
        if (property.isMap) {
            // a map always has string keys
            sb.append("String), FullType(");
        }
        if (property.items.isContainer) {
            appendCollection(sb, property.items);
        } else {
            sb.append(property.items.datatypeWithEnum);
        }
        sb.append(")]");
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        Set<Map<String, Object>> serializers = new HashSet<>();
        Set<String> resultImports = new HashSet<>();

        for (CodegenOperation op : operationList) {
            for (CodegenParameter param : op.bodyParams) {
                if (param.baseType != null && param.baseType.equalsIgnoreCase("Uint8List") && op.isMultipart) {
                    param.baseType = "MultipartFile";
                    param.dataType = "MultipartFile";
                }
                if (param.isContainer) {
                    final Map<String, Object> serializer = new HashMap<>();
                    serializer.put("isArray", param.isArray);
                    serializer.put("uniqueItems", param.uniqueItems);
                    serializer.put("isMap", param.isMap);
                    serializer.put("baseType", param.baseType);
                    serializers.add(serializer);
                }
            }

            resultImports.addAll(rewriteImports(op.imports));
            if (op.getHasFormParams()) {
                resultImports.add("package:" + pubName + "/api_util.dart");
            }

            if (op.returnContainer != null) {
                final Map<String, Object> serializer = new HashMap<>();
                serializer.put("isArray", Objects.equals("array", op.returnContainer) || Objects.equals("set", op.returnContainer));
                serializer.put("uniqueItems", op.uniqueItems);
                serializer.put("isMap", Objects.equals("map", op.returnContainer));
                serializer.put("baseType", op.returnBaseType);
                serializers.add(serializer);
            }
        }

        objs.put("imports", resultImports.stream().sorted().collect(Collectors.toList()));
        objs.put("serializers", serializers);

        return objs;
    }

    private Set<String> rewriteImports(Set<String> originalImports) {
        Set<String> resultImports = Sets.newHashSet();
        for (String modelImport : originalImports) {
            if (imports.containsKey(modelImport)) {
                resultImports.add(imports.get(modelImport));
            } else {
                resultImports.add("package:" + pubName + "/model/" + underscore(modelImport) + ".dart");
            }
        }
        return resultImports;
    }
}
