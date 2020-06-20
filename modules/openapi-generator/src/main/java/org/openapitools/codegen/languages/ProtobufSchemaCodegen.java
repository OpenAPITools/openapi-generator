/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.utils.ProcessUtils;
import org.openapitools.codegen.utils.ModelUtils;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class ProtobufSchemaCodegen extends DefaultCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(ProtobufSchemaCodegen.class);

    protected String packageName = "openapitools";

    @Override
    public CodegenType getTag() {
        return CodegenType.CONFIG;
    }

    public String getName() {
        return "protobuf-schema";
    }

    public String getHelp() {
        return "Generates gRPC and protocol buffer schema files (beta)";
    }

    public ProtobufSchemaCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .includeWireFormatFeatures(WireFormatFeature.PROTOBUF)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.PROTOBUF))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
        );

        outputFolder = "generated-code/protobuf-schema";
        modelTemplateFiles.put("model.mustache", ".proto");
        apiTemplateFiles.put("api.mustache", ".proto");
        embeddedTemplateDir = templateDir = "protobuf-schema";
        hideGenerationTimestamp = Boolean.TRUE;
        modelPackage = "messages";
        apiPackage = "services";

        /*setReservedWordsLowerCase(
                Arrays.asList(
                        // data type
                        "nil", "string", "boolean", "number", "userdata", "thread",
                        "table",

                        // reserved words: http://www.lua.org/manual/5.1/manual.html#2.1
                        "and", "break", "do", "else", "elseif",
                        "end", "false", "for", "function", "if",
                        "in", "local", "nil", "not", "or",
                        "repeat", "return", "then", "true", "until", "while"
                )
        );*/

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                        "map",
                        "array")
        );

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "map",
                        "array",
                        "bool",
                        "bytes",
                        "string",
                        "int32",
                        "int64",
                        "uint32",
                        "uint64",
                        "sint32",
                        "sint64",
                        "fixed32",
                        "fixed64",
                        "sfixed32",
                        "sfixed64",
                        "float",
                        "double")
        );

        instantiationTypes.clear();
        instantiationTypes.put("array", "repeat");
        //instantiationTypes.put("map", "map");

        // ref: https://developers.google.com/protocol-buffers/docs/proto
        typeMapping.clear();
        typeMapping.put("array", "array");
        typeMapping.put("map", "map");
        typeMapping.put("integer", "int32");
        typeMapping.put("long", "int64");
        typeMapping.put("number", "float");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "string");
        typeMapping.put("password", "string");
        // TODO fix file mapping
        typeMapping.put("file", "string");
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "bytes");
        typeMapping.put("object", "TODO_OBJECT_MAPPING");

        importMapping.clear(); 
        /* 
        importMapping = new HashMap<String, String>();
        importMapping.put("time.Time", "time");
        importMapping.put("*os.File", "os");
        importMapping.put("os", "io/ioutil");
        */

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        cliOptions.clear();
        /*cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "GraphQL package name (convention: lowercase).")
                .defaultValue("openapi2graphql"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "GraphQL package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));*/

    }

    @Override
    public void processOpts() {
        super.processOpts();

        //apiTestTemplateFiles.put("api_test.mustache", ".proto");
        //modelTestTemplateFiles.put("model_test.mustache", ".proto");

        apiDocTemplateFiles.clear(); // TODO: add api doc template
        modelDocTemplateFiles.clear(); // TODO: add model doc template

        modelPackage = "models";
        apiPackage = "services";

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        //supportingFiles.add(new SupportingFile("root.mustache", "", packageName + ".proto"));
        //supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        //supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"))
        //supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty (should not occur as an auto-generated method name will be used)
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return camelize(sanitizeName(operationId));
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = postProcessModelsEnum(objs);
        List<Object> models = (List<Object>) objs.get("models");
        // add x-index to properties
        ProcessUtils.addIndexToProperties(models, 1);

        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            for (CodegenProperty var : cm.vars) {
                // add x-protobuf-type: repeated if it's an array
                if (Boolean.TRUE.equals(var.isListContainer)) {
                    var.vendorExtensions.put("x-protobuf-type", "repeated");
                }

                // add x-protobuf-data-type
                // ref: https://developers.google.com/protocol-buffers/docs/proto3
                if (!var.vendorExtensions.containsKey("x-protobuf-data-type")) {
                    if (var.isListContainer) {
                        var.vendorExtensions.put("x-protobuf-data-type", var.items.dataType);
                    } else {
                        var.vendorExtensions.put("x-protobuf-data-type", var.dataType);
                    }
                }

                if (var.isEnum && var.allowableValues.containsKey("enumVars")) {
                    List<Map<String, Object>> enumVars = (List<Map<String, Object>>) var.allowableValues.get("enumVars");
                    int enumIndex = 0;
                    for (Map<String, Object> enumVar : enumVars) {
                        enumVar.put("protobuf-enum-index", enumIndex);
                        enumIndex++;
                    }
                }
            }
        }
        return objs;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input;
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input;
    }

    /**
     * Return the default value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                if (Boolean.valueOf(p.getDefault().toString()) == false)
                    return "false";
                else
                    return "true";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            // TODO
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                if (Pattern.compile("\r\n|\r|\n").matcher((String) p.getDefault()).find())
                    return "'''" + p.getDefault() + "'''";
                else
                    return "'" + p.getDefault() + "'";
            }
        } else if (ModelUtils.isArraySchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        }

        return null;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separatorChar + apiPackage;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separatorChar + modelPackage;
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // e.g. PhoneNumber => phone_number
        return underscore(name) + "_service";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultService";
        }
        // e.g. phone_number => PhoneNumber
        return camelize(name) + "Service";
    }

    @Override
    public String toApiVarName(String name) {
        if (name.length() == 0) {
            return "default_service";
        }
        return underscore(name) + "_service";
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    @Override
    public String toModelFilename(String name) {
        // underscore the model file name
        // PhoneNumber => phone_number
        return underscore(toModelName(name));
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // remove dollar sign
        name = name.replaceAll("$", "");

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = toModelName(schemaType);
        }
        return type;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            int index = 1;
            for (CodegenParameter p : op.allParams) {
                // add x-protobuf-type: repeated if it's an array
                if (Boolean.TRUE.equals(p.isListContainer)) {
                    p.vendorExtensions.put("x-protobuf-type", "repeated");
                } else if (Boolean.TRUE.equals(p.isMapContainer)) {
                    LOGGER.warn("Map parameter (name: {}, operation ID: {}) not yet supported", p.paramName, op.operationId);
                }

                // add x-protobuf-data-type
                // ref: https://developers.google.com/protocol-buffers/docs/proto3
                if (!p.vendorExtensions.containsKey("x-protobuf-data-type")) {
                    if (Boolean.TRUE.equals(p.isListContainer)) {
                        p.vendorExtensions.put("x-protobuf-data-type", p.items.dataType);
                    } else {
                        p.vendorExtensions.put("x-protobuf-data-type", p.dataType);
                    }
                }

                p.vendorExtensions.put("x-index", index);
                index++;
            }

            if (StringUtils.isEmpty(op.returnType)) {
                op.vendorExtensions.put("x-grpc-response", "google.protobuf.Empty");
            } else {
                if (Boolean.FALSE.equals(op.returnTypeIsPrimitive) && StringUtils.isEmpty(op.returnContainer)) {
                    op.vendorExtensions.put("x-grpc-response", op.returnType);
                } else {
                    if ("map".equals(op.returnContainer)) {
                        LOGGER.warn("Map response (operation ID: {}) not yet supported", op.operationId);
                        op.vendorExtensions.put("x-grpc-response-type", op.returnBaseType);
                    } else if ("array".equals(op.returnContainer)) {
                        op.vendorExtensions.put("x-grpc-response-type", "repeated " + op.returnBaseType);
                    } else { // primitive type
                        op.vendorExtensions.put("x-grpc-response-type", op.returnBaseType);
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public String toModelImport(String name) {
        return underscore(name);
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
}
