/*
 * Copyright 2021 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.ClientModificationFeature;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.utils.StringUtils.underscore;

public class DartDioNextClientCodegen extends AbstractDartCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(DartDioNextClientCodegen.class);

    public static final String DATE_LIBRARY = "dateLibrary";
    public static final String DATE_LIBRARY_CORE = "core";
    public static final String DATE_LIBRARY_TIME_MACHINE = "timemachine";
    public static final String DATE_LIBRARY_DEFAULT = DATE_LIBRARY_CORE;

    public static final String SERIALIZATION_LIBRARY_BUILT_VALUE = "built_value";
    public static final String SERIALIZATION_LIBRARY_DEFAULT = SERIALIZATION_LIBRARY_BUILT_VALUE;

    private static final String DIO_IMPORT = "package:dio/dio.dart";
    private static final String CLIENT_NAME = "clientName";

    private String dateLibrary;

    private String clientName;

    public DartDioNextClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeClientModificationFeatures(
                        ClientModificationFeature.Authorizations,
                        ClientModificationFeature.UserAgent
                )
        );
        generatorMetadata = GeneratorMetadata.newBuilder()
                .stability(Stability.EXPERIMENTAL)
                .build();

        outputFolder = "generated-code/dart-dio-next";
        embeddedTemplateDir = "dart/libraries/dio";
        this.setTemplateDir(embeddedTemplateDir);

        apiPackage = "lib.src.api";
        modelPackage = "lib.src.model";

        supportedLibraries.put(SERIALIZATION_LIBRARY_BUILT_VALUE, "[DEFAULT] built_value");
        final CliOption serializationLibrary = CliOption.newString(CodegenConstants.SERIALIZATION_LIBRARY, "Specify serialization library");
        serializationLibrary.setEnum(supportedLibraries);
        serializationLibrary.setDefault(SERIALIZATION_LIBRARY_DEFAULT);
        cliOptions.add(serializationLibrary);

        final CliOption dateOption = CliOption.newString(DATE_LIBRARY, "Specify Date library");
        dateOption.setDefault(DATE_LIBRARY_DEFAULT);

        final Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(DATE_LIBRARY_CORE, "[DEFAULT] Dart core library (DateTime)");
        dateOptions.put(DATE_LIBRARY_TIME_MACHINE, "Time Machine is date and time library for Flutter, Web, and Server with support for timezones, calendars, cultures, formatting and parsing.");
        dateOption.setEnum(dateOptions);
        cliOptions.add(dateOption);
    }

    public String getDateLibrary() {
        return dateLibrary;
    }

    public void setDateLibrary(String library) {
        this.dateLibrary = library;
    }

    public String getClientName() {
        return clientName;
    }

    public void setClientName(String clientName) {
        this.clientName = clientName;
    }

    @Override
    public String getName() {
        return "dart-dio-next";
    }

    @Override
    public String getHelp() {
        return "Generates a Dart Dio client library with null-safety.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("DART_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable DART_POST_PROCESS_FILE not defined so the Dart code may not be properly formatted. To define it, try `export DART_POST_PROCESS_FILE=\"/usr/local/bin/dartfmt -w\"` (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (!additionalProperties.containsKey(CodegenConstants.SERIALIZATION_LIBRARY)) {
            additionalProperties.put(CodegenConstants.SERIALIZATION_LIBRARY, SERIALIZATION_LIBRARY_DEFAULT);
            LOGGER.debug("Serialization library not set, using default {}", SERIALIZATION_LIBRARY_DEFAULT);
        }
        setLibrary(additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY).toString());

        if (!additionalProperties.containsKey(DATE_LIBRARY)) {
            additionalProperties.put(DATE_LIBRARY, DATE_LIBRARY_DEFAULT);
            LOGGER.debug("Date library not set, using default {}", DATE_LIBRARY_DEFAULT);
        }
        setDateLibrary(additionalProperties.get(DATE_LIBRARY).toString());

        if (!additionalProperties.containsKey(CLIENT_NAME)) {
            final String name = org.openapitools.codegen.utils.StringUtils.camelize(pubName);
            additionalProperties.put(CLIENT_NAME, name);
            LOGGER.debug("Client name not set, using default {}", DATE_LIBRARY_DEFAULT);
        }
        setClientName(additionalProperties.get(CLIENT_NAME).toString());

        supportingFiles.add(new SupportingFile("pubspec.mustache", "", "pubspec.yaml"));
        supportingFiles.add(new SupportingFile("analysis_options.mustache", "", "analysis_options.yaml"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        final String libFolder = sourceFolder + File.separator + "lib";
        supportingFiles.add(new SupportingFile("lib.mustache", libFolder, pubName + ".dart"));

        final String srcFolder = libFolder + File.separator + "src";
        supportingFiles.add(new SupportingFile("api_client.mustache", srcFolder, "api.dart"));

        final String authFolder = srcFolder + File.separator + "auth";
        supportingFiles.add(new SupportingFile("auth/api_key_auth.mustache", authFolder, "api_key_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/basic_auth.mustache", authFolder, "basic_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/bearer_auth.mustache", authFolder, "bearer_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/oauth.mustache", authFolder, "oauth.dart"));
        supportingFiles.add(new SupportingFile("auth/auth.mustache", authFolder, "auth.dart"));

        configureSerializationLibrary(srcFolder);
        configureDateLibrary(srcFolder);
    }

    private void configureSerializationLibrary(String srcFolder) {
        switch (library) {
            default:
            case SERIALIZATION_LIBRARY_BUILT_VALUE:
                additionalProperties.put("useBuiltValue", "true");
                configureSerializationLibraryBuiltValue(srcFolder);
                break;
        }
    }

    private void configureSerializationLibraryBuiltValue(String srcFolder) {
        supportingFiles.add(new SupportingFile("serialization/built_value/serializers.mustache", srcFolder, "serializers.dart"));
        supportingFiles.add(new SupportingFile("serialization/built_value/api_util.mustache", srcFolder, "api_util.dart"));

        typeMapping.put("Array", "BuiltList");
        typeMapping.put("array", "BuiltList");
        typeMapping.put("List", "BuiltList");
        typeMapping.put("set", "BuiltSet");
        typeMapping.put("map", "BuiltMap");
        typeMapping.put("file", "MultipartFile");
        typeMapping.put("binary", "MultipartFile");
        typeMapping.put("object", "JsonObject");
        typeMapping.put("AnyType", "JsonObject");

        imports.put("BuiltList", "package:built_collection/built_collection.dart");
        imports.put("BuiltSet", "package:built_collection/built_collection.dart");
        imports.put("BuiltMap", "package:built_collection/built_collection.dart");
        imports.put("JsonObject", "package:built_value/json_object.dart");
        imports.put("Uint8List", "dart:typed_data");
        imports.put("MultipartFile", DIO_IMPORT);
    }

    private void configureDateLibrary(String srcFolder) {
        switch (dateLibrary) {
            case DATE_LIBRARY_TIME_MACHINE:
                additionalProperties.put("useDateLibTimeMachine", "true");
                typeMapping.put("date", "OffsetDate");
                typeMapping.put("Date", "OffsetDate");
                typeMapping.put("DateTime", "OffsetDateTime");
                typeMapping.put("datetime", "OffsetDateTime");
                imports.put("OffsetDate", "package:time_machine/time_machine.dart");
                imports.put("OffsetDateTime", "package:time_machine/time_machine.dart");
                if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
                    supportingFiles.add(new SupportingFile("serialization/built_value/offset_date_serializer.mustache", srcFolder, "local_date_serializer.dart"));
                }
                break;
            default:
            case DATE_LIBRARY_CORE:
                additionalProperties.put("useDateLibCore", "true");
                if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
                    typeMapping.put("date", "Date");
                    typeMapping.put("Date", "Date");
                    importMapping.put("Date", "package:" + pubName + "/src/model/date.dart");
                    supportingFiles.add(new SupportingFile("serialization/built_value/date.mustache", srcFolder + File.separator + "model", "date.dart"));
                    supportingFiles.add(new SupportingFile("serialization/built_value/date_serializer.mustache", srcFolder, "date_serializer.dart"));
                }
                break;
        }
    }

    @Override
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
                if (ModelUtils.isArraySchema(schema)) {
                    if (ModelUtils.isSet(schema)) {
                        return "SetBuilder()";
                    }
                    return "ListBuilder()";
                }
                if (ModelUtils.isMapSchema(schema)) {
                    return "MapBuilder()";
                }
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
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = super.postProcessModels(objs);
        List<Object> models = (List<Object>) objs.get("models");
        ProcessUtils.addIndexToProperties(models, 1);

        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = rewriteImports(cm.imports, true);
            cm.vendorExtensions.put("x-has-vars", !cm.vars.isEmpty());
        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
            if (property.isEnum) {
                // enums are generated with built_value and make use of BuiltSet
                model.imports.add("BuiltSet");
            }
        }
    }

    /*This does 3 things
        1) makes sure that inline enums are unique per operation by appending opName to the enum name
        2) Solves a bug where if we have a container of enum, the "allowableValues" property doesn't get passed down to the "items" property
        3) provides defaultValueWithEnum as a vendor ext which gives full information about the default value, including its name
    */
    private void fixEnumParam(String opName,IJsonSchemaValidationProperties param) {
        CodegenProperty items;
        CodegenProperty mostInnerItems;
        boolean isEnum;
        boolean isContainer;
        Map<String, Object> allowableValues;
        String defaultValue;
        String enumName;
        Map<String, Object> vendorExt;
        if (param instanceof CodegenParameter) {
            final CodegenParameter castedParam = (CodegenParameter)param;
            allowableValues = castedParam.allowableValues;
            isEnum = castedParam.isEnum;
            isContainer = castedParam.isContainer;
            defaultValue = castedParam.defaultValue;
            vendorExt = castedParam.vendorExtensions;
            enumName = castedParam.enumName;
            mostInnerItems = castedParam.mostInnerItems;
            items = castedParam.items;
        } else if (param instanceof CodegenProperty) {
            final CodegenProperty castedParam = (CodegenProperty)param;
            allowableValues = castedParam.allowableValues;
            isEnum = castedParam.isEnum;
            isContainer = castedParam.isContainer;
            defaultValue = castedParam.defaultValue;
            vendorExt = castedParam.vendorExtensions;
            enumName = castedParam.enumName;
            mostInnerItems = castedParam.mostInnerItems;
            items = castedParam.items;
        }
        else {
            return;
        }
        if (isEnum) {
            if (param instanceof CodegenParameter) { 
                final CodegenParameter castedParam = (CodegenParameter)param;
                if (!castedParam.enumName.contains(opName)) {
                    enumName = castedParam.enumName += opName;                    
                }
            } else {
                final CodegenProperty castedParam = (CodegenProperty)param;
                if (!castedParam.enumName.contains(opName)) {
                    enumName = castedParam.enumName += opName;                    
                }
            }
            if (isContainer) {
                if (items != null) {
                    items.setAllowableValues(allowableValues);
                    items.enumName = enumName;
                    fixEnumParam(opName, items);
                }
                if (mostInnerItems != null) {
                    mostInnerItems.setAllowableValues(allowableValues);
                    mostInnerItems.enumName = enumName; 
                    fixEnumParam(opName, mostInnerItems);
                }
                
            } else {
                if (!StringUtils.isEmpty(defaultValue)) {
                    final List<Object> enumVars = (List<Object>) allowableValues.get("enumVars");
                    
                    Map<String,Object> defaultEnumVar = enumVars
                        .stream()
                        .map(enumVar -> (Map<String,Object>)enumVar)
                        .filter(enumVar -> enumVar.get("value").equals(defaultValue))
                        .findFirst()
                        .orElse(null);
                    vendorExt.put("defaultValueWithEnum", defaultEnumVar);
                }
            }
        } else {
            if (isContainer) {
                if (items != null) {
                    fixEnumParam(opName, items);
                }
                if (mostInnerItems != null){
                    fixEnumParam(opName, mostInnerItems);
                }
            }
        }
      
    }
    /// A workaround for https://github.com/OpenAPITools/openapi-generator/issues/10189
    private void fixOperationParam(String opName, CodegenParameter param) {
        if (param.isContainer) {
            if (param.isArray) {
                if (param.uniqueItems) {
                    param.baseType = typeMapping.get("set");
                } else {
                    param.baseType = typeMapping.get("array");
                }
            } else if (param.isMap) {
                param.baseType = typeMapping.get("map");
            }
        }
        fixEnumParam(opName, param);        
    }

    /// A workaround for https://github.com/OpenAPITools/openapi-generator/issues/10189
    private void fixOperationResponse(CodegenResponse response) {        
        if (response.isContainer) {
            if (response.isArray) {
                if (response.getUniqueItems()) {
                    response.baseType = typeMapping.get("set");
                } else {
                    response.baseType = typeMapping.get("array");
                }
            } else if (response.isMap) {
                response.baseType = typeMapping.get("map");
            }    
        }        
    }
    private Map<String,Object> getCodegenParameterTypeData(CodegenParameter param) {
        final Map<String, Object> typeData = new HashMap<>();
        
        typeData.put("isArray", param.getIsArray());
        typeData.put("uniqueItems", param.getUniqueItems());
        typeData.put("isMap", param.getIsMap());
        typeData.put("isContainer", param.isContainer);
        typeData.put("baseType", param.baseType);        
        if (param.items != null) {
            typeData.put("items", param.items);
        }
        return typeData;
    }
    private Map<String,Object> getCodegenResponseTypeData(CodegenResponse r) {
        final Map<String, Object> typeData = new HashMap<>();
        
        typeData.put("isArray", r.getIsArray());
        typeData.put("uniqueItems", r.getUniqueItems());
        typeData.put("isMap", r.getIsMap());
        typeData.put("isContainer", r.isContainer);
        typeData.put("baseType", r.baseType);        
        if (r.items != null) {
            typeData.put("items", r.items);
        }
        return typeData;
    }

    //Extracts inline enums to put them later in an "inlineEnums" property for each api
    private void extractInlineEnums(IJsonSchemaValidationProperties property, Map<String, Object> inlineEnums) {
        if (property instanceof CodegenProperty) {
            final CodegenProperty casted = (CodegenProperty) property;
            if (casted.isEnum) {
                if (!inlineEnums.containsKey(casted.enumName)) {
                    inlineEnums.put(casted.enumName, casted);                 
                }
            }
        } else if (property instanceof CodegenParameter) {
            final CodegenParameter casted = (CodegenParameter) property;
            if (casted.isEnum) {
                if (!inlineEnums.containsKey(casted.enumName)) {
                    inlineEnums.put(casted.enumName, casted);                 
                }
            }
        }
        final CodegenProperty items = property.getItems();
        if (items != null) {
            if (items.isContainer) {
                extractInlineEnums(items.items, inlineEnums);
            } else if (items.isEnum) {
                extractInlineEnums(items, inlineEnums);
            }
        }
    }
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        Set<Map<String, Object>> serializers = new HashSet<>();
        Map<String, Object> inlineEnums = new HashMap<>();
        Set<String> resultImports = new HashSet<>();
        for (CodegenOperation op : operationList) {            
            List<CodegenParameter> actualAllParams = Stream.of(
                op.allParams,
                op.bodyParams,
                op.pathParams,
                op.queryParams,
                op.headerParams,
                op.formParams,                
                op.cookieParams,
                op.requiredParams,
                op.optionalParams).flatMap(Collection::stream).collect(Collectors.toList());
            if (op.bodyParam != null) {
                actualAllParams.add(op.bodyParam);
            }
            for (CodegenParameter param : actualAllParams) {
                fixOperationParam(op.operationId, param);  
                if (param.isContainer && !(param.isBinary || param.isFile)) {                    
                    serializers.add(getCodegenParameterTypeData(param));
                }
                extractInlineEnums(param, inlineEnums);
            }
            for (CodegenResponse response : op.responses) {
                fixOperationResponse(response);
                if (response.isContainer && !(response.isBinary || response.isFile)) {                    
                    serializers.add(getCodegenResponseTypeData(response));
                }                
            }
            resultImports.addAll(rewriteImports(op.imports, false));
            if (op.getHasFormParams() || op.getHasQueryParams()) {
                resultImports.add("package:" + pubName + "/src/api_util.dart");
            }
        }
        if (!inlineEnums.isEmpty()) {
            resultImports.add("package:built_value/built_value.dart");
        }
        objs.put("imports", resultImports.stream().sorted().collect(Collectors.toList()));
        objs.put("serializers", serializers);
        objs.put("inlineEnums", inlineEnums.values());
        objs.put("hasInlineEnums", !inlineEnums.isEmpty());

        return objs;
    }

    private void addBuiltValueSerializerImport(String type) {
        additionalProperties.compute("builtValueSerializerImports", (k, v) -> {
            Set<String> imports = v == null ? Sets.newHashSet() : ((Set<String>) v);
            imports.addAll(rewriteImports(Sets.newHashSet(type), true));
            return imports;
        });
    }

    private Set<String> rewriteImports(Set<String> originalImports, boolean isModel) {
        Set<String> resultImports = Sets.newHashSet();
        for (String modelImport : originalImports) {
            if (imports.containsKey(modelImport)) {
                String i = imports.get(modelImport);
                if (Objects.equals(i, DIO_IMPORT) && !isModel) {
                    // Don't add imports to operations that are already imported
                    continue;
                }
                resultImports.add(i);
            } else {
                resultImports.add("package:" + pubName + "/src/model/" + underscore(modelImport) + ".dart");
            }
        }
        return resultImports;
    }
}
