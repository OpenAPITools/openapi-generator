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

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.function.Predicate;

import static org.openapitools.codegen.utils.StringUtils.underscore;

public class CppOatppClientCodegen extends AbstractCppCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(CppOatppClientCodegen.class);

    protected boolean isAddExternalLibs = true;
    public static final String OPTIONAL_EXTERNAL_LIB = "addExternalLibs";
    public static final String OPTIONAL_EXTERNAL_LIB_DESC = "Add the Possibility to fetch and compile external Libraries needed by this Framework.";
    protected final String PREFIX = "";

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "cpp-oatpp-client";
    }

    @Override
    public String getHelp() {
        return "Generates a C++ API client (based on Oat++)";
    }

    public CppOatppClientCodegen() {
        super();

        // TODO: cpp-oatpp-client maintainer review
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer)
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism)
                .excludeParameterFeatures(
                        ParameterFeature.Cookie));

        if (StringUtils.isEmpty(modelNamePrefix)) {
            modelNamePrefix = PREFIX;
        }

        apiPackage = "org.openapitools.client.api";
        modelPackage = "org.openapitools.client.model";

        apiTemplateFiles.put("api-header.mustache", ".hpp");

        modelTemplateFiles.put("model-header.mustache", ".hpp");

        embeddedTemplateDir = templateDir = "cpp-oatpp-client";

        cliOptions.clear();
        addSwitch(OPTIONAL_EXTERNAL_LIB, OPTIONAL_EXTERNAL_LIB_DESC, this.isAddExternalLibs);
        addOption(RESERVED_WORD_PREFIX_OPTION, RESERVED_WORD_PREFIX_DESC, this.reservedWordPrefix);
        addOption(VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_OPTION,
                VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_DESC,
                Boolean.toString(this.variableNameFirstCharacterUppercase));

        setupSupportingFiles();

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "oatpp::String",
                        "oatpp::Boolean",
                        "oatpp::Int32",
                        "oatpp::Int64",
                        "oatpp::Vector",
                        "oatpp::Fields",
                        "oatpp::UnorderedSet",
                        "oatpp::Object",
                        "oatpp::Float64",
                        "oatpp::Any"
                ));

        typeMapping = new HashMap<>();
        typeMapping.put("date", "oatpp::String");
        typeMapping.put("DateTime", "oatpp::String");
        typeMapping.put("string", "oatpp::String");
        typeMapping.put("integer", "oatpp::Int32");
        typeMapping.put("long", "oatpp::Int64");
        typeMapping.put("boolean", "oatpp::Boolean");
        typeMapping.put("array", "oatpp::Vector");
        typeMapping.put("map", "oatpp::Fields");
        typeMapping.put("set", "oatpp::UnorderedSet");
        typeMapping.put("file", "oatpp::String");
        typeMapping.put("object", "oatpp::Object");
        typeMapping.put("binary", "oatpp::String");
        typeMapping.put("number", "oatpp::Float64");
        typeMapping.put("UUID", "oatpp::String");
        typeMapping.put("URI", "oatpp::String");
        typeMapping.put("ByteArray", "oatpp::String");
        typeMapping.put("AnyType", "oatpp::Any");

        super.importMapping = new HashMap<>();
    }

    private void setupSupportingFiles() {
        supportingFiles.clear();
        supportingFiles
                .add(new SupportingFile("main-api-client.mustache", "", modelNamePrefix + "main-api-client.cpp"));
        supportingFiles.add(new SupportingFile("cmake.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey("modelNamePrefix")) {
            additionalProperties().put("prefix", modelNamePrefix);
            setupSupportingFiles();
        }
        if (additionalProperties.containsKey(RESERVED_WORD_PREFIX_OPTION)) {
            reservedWordPrefix = (String) additionalProperties.get(RESERVED_WORD_PREFIX_OPTION);
        }

        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
        additionalProperties.put(RESERVED_WORD_PREFIX_OPTION, reservedWordPrefix);

        if (additionalProperties.containsKey(OPTIONAL_EXTERNAL_LIB)) {
            setAddExternalLibs(convertPropertyToBooleanAndWriteBack(OPTIONAL_EXTERNAL_LIB));
        } else {
            additionalProperties.put(OPTIONAL_EXTERNAL_LIB, isAddExternalLibs);
        }
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        } else {
            return "#include \"" + name + ".hpp\"";
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);

        Set<String> oldImports = codegenModel.imports;
        codegenModel.imports = new HashSet<>();
        for (String imp : oldImports) {
            String newImp = toModelImport(imp);
            if (!newImp.isEmpty()) {
                codegenModel.imports.add(newImp);
            }
        }

        if (!codegenModel.isEnum
                && codegenModel.anyOf.size() > 1
                && codegenModel.anyOf.contains("std::string")
                && !codegenModel.anyOf.contains("AnyType")
                && codegenModel.interfaces.size() == 1) {
            codegenModel.vendorExtensions.put("x-is-string-enum-container", true);
        }
        return codegenModel;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse apiResponse = findMethodResponse(operation.getResponses());

            if (apiResponse != null) {
                Schema response = ModelUtils.getSchemaFromResponse(openAPI, apiResponse);
                if (response != null) {
                    CodegenProperty cm = fromProperty("response", response, false);
                    op.vendorExtensions.put("x-codegen-response", cm);
                    if ("HttpContent".equals(cm.dataType)) {
                        op.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                    }
                }
            }
        }

        String pathForOatpp = path.replaceAll("\\{(.*?)}", "{$1}");
        op.vendorExtensions.put("x-codegen-oatpp-path", pathForOatpp);

        return op;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap operations = objs.getOperations();
        String classname = operations.getClassname();
        operations.put("classnameSnakeUpperCase", underscore(classname).toUpperCase(Locale.ROOT));
        operations.put("classnameSnakeLowerCase", underscore(classname).toLowerCase(Locale.ROOT));
        List<CodegenOperation> operationList = operations.getOperation();
        for (CodegenOperation op : operationList) {
            postProcessSingleOperation(operations, op);
        }

        return objs;
    }

    private void postProcessSingleOperation(OperationMap operations, CodegenOperation op) {
        if (op.vendorExtensions == null) {
            op.vendorExtensions = new HashMap<>();
        }

        if (op.bodyParam != null) {
            if (op.bodyParam.vendorExtensions == null) {
                op.bodyParam.vendorExtensions = new HashMap<>();
            }

            boolean isStringOrDate = op.bodyParam.isString || op.bodyParam.isDate;
            op.bodyParam.vendorExtensions.put("x-codegen-oatpp-is-string-or-date", isStringOrDate);
        }

        boolean consumeJson = false;
        if (op.consumes != null) {
            Predicate<Map<String, String>> isMediaTypeJson = consume -> (consume.get("mediaType") != null
                    && consume.get("mediaType").equals("application/json"));
            consumeJson = op.consumes.stream().anyMatch(isMediaTypeJson);
        }
        op.vendorExtensions.put("x-codegen-oatpp-consumes-json", consumeJson);

        // Check if any one of the operations needs a model, then at API file level, at
        // least one model has to be included.
        Predicate<String> importNotInImportMapping = hdr -> !importMapping.containsKey(hdr);
        if (op.imports.stream().anyMatch(importNotInImportMapping)) {
            operations.put("hasModelImport", true);
        }
    }

    /**
     * postProcessSingleParam - Modifies a single parameter, adjusting generated
     * data types for Header and Query parameters.
     * 
     * @param param CodegenParameter to be modified.
     */
    private static void postProcessSingleParam(CodegenParameter param) {
        if (param.isQueryParam) {
            param.dataType = "std::optional<" + param.dataType + ">";
            if (!param.isPrimitiveType) {
                param.baseType = "std::optional<" + param.baseType + ">";
            }
        }
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        return super.apiFilename(templateName, tag);
    }

    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    /**
     * Optional - type declaration. This is a String which is used by the
     * templates to instantiate your types. There is typically special handling
     * for different property types
     *
     * @return a string value used as the `dataType` field for model templates,
     *         `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            if (languageSpecificPrimitives.contains(getSchemaType(inner))) {
                return getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">";
            }
            return getSchemaType(p) + "<oatpp::Object<" + getTypeDeclaration(inner) + ">>";
        }
        if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return getSchemaType(p) + "<std::string, " + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isByteArraySchema(p)) {
            return "std::string";
        }
        if (ModelUtils.isStringSchema(p)
                || ModelUtils.isDateSchema(p)
                || ModelUtils.isDateTimeSchema(p) || ModelUtils.isFileSchema(p)
                || languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        }

        String namespace = (String) additionalProperties.get("modelNamespace");
        return namespace + "::" + openAPIType;
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "false";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isDateTimeSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (ModelUtils.isFloatSchema(p)) { // float
                if (p.getDefault() != null) {
                    // We have to ensure that our default value has a decimal point,
                    // because in C++ the 'f' suffix is not valid on integer literals
                    // i.e. 374.0f is a valid float but 374 isn't.
                    String defaultStr = p.getDefault().toString();
                    if (defaultStr.indexOf('.') < 0) {
                        return defaultStr + ".0f";
                    } else {
                        return defaultStr + "f";
                    }
                } else {
                    return "0.0f";
                }
            } else { // double
                if (p.getDefault() != null) {
                    return p.getDefault().toString();
                } else {
                    return "0.0";
                }
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (ModelUtils.isLongSchema(p)) { // long
                if (p.getDefault() != null) {
                    return p.getDefault().toString() + "L";
                } else {
                    return "0L";
                }
            } else { // integer
                if (p.getDefault() != null) {
                    return p.getDefault().toString();
                } else {
                    return "0";
                }
            }
        } else if (ModelUtils.isByteArraySchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(ModelUtils.getAdditionalProperties(p));
            return "std::map<std::string, " + inner + ">()";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            if (!languageSpecificPrimitives.contains(inner)) {
                inner = "std::shared_ptr<" + inner + ">";
            }
            return "std::vector<" + inner + ">()";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            return "std::make_shared<" + toModelName(ModelUtils.getSimpleRef(p.get$ref())) + ">()";
        }

        return "nullptr";
    }

    /**
     * Location to write model files. You can use the modelPackage() as defined
     * when the class is instantiated
     */
    @Override
    public String modelFileFolder() {
        return (outputFolder + "/model").replace("/", File.separator);
    }

    /**
     * Location to write api files. You can use the apiPackage() as defined when
     * the class is instantiated
     */
    @Override
    public String apiFileFolder() {
        return (outputFolder + "/api").replace("/", File.separator);
    }

    /**
     * Optional - OpenAPI type conversion. This is used to map OpenAPI types in
     * a `Schema` into either language specific types via `typeMapping` or
     * into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */
    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public String getTypeDeclaration(String str) {
        return toModelName(str);
    }

    /**
     * Specify whether external libraries will be added during the generation
     *
     * @param value the value to be set
     */
    public void setAddExternalLibs(boolean value) {
        isAddExternalLibs = value;
    }
}
