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

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.underscore;

public class CppPistacheServerCodegen extends AbstractCppCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(CppPistacheServerCodegen.class);

    protected String implFolder = "impl";
    protected boolean isAddExternalLibs = true;
    protected boolean isUseStructModel = false;
    public static final String OPTIONAL_EXTERNAL_LIB = "addExternalLibs";
    public static final String OPTIONAL_EXTERNAL_LIB_DESC = "Add the Possibility to fetch and compile external Libraries needed by this Framework.";
    public static final String OPTION_USE_STRUCT_MODEL = "useStructModel";
    public static final String OPTION_USE_STRUCT_MODEL_DESC = "Use struct-based model template instead of get/set-based model template";
    public static final String HELPERS_PACKAGE_NAME = "helpersPackage";
    public static final String HELPERS_PACKAGE_NAME_DESC = "Specify the package name to be used for the helpers (e.g. org.openapitools.server.helpers).";
    protected final String PREFIX = "";
    protected String helpersPackage = "";

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "cpp-pistache-server";
    }

    @Override
    public String getHelp() {
        return "Generates a C++ API server (based on Pistache)";
    }

    public CppPistacheServerCodegen() {
        super();

        // TODO: cpp-pistache-server maintainer review
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        if (StringUtils.isEmpty(modelNamePrefix)) {
            modelNamePrefix = PREFIX;
        }

        helpersPackage = "org.openapitools.server.helpers";
        apiPackage = "org.openapitools.server.api";
        modelPackage = "org.openapitools.server.model";

        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");
        apiTemplateFiles.put("api-impl-header.mustache", ".h");
        apiTemplateFiles.put("api-impl-source.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "cpp-pistache-server";

        cliOptions.clear();
        addSwitch(OPTIONAL_EXTERNAL_LIB, OPTIONAL_EXTERNAL_LIB_DESC, this.isAddExternalLibs);
        addOption(HELPERS_PACKAGE_NAME, HELPERS_PACKAGE_NAME_DESC, this.helpersPackage);
        addOption(RESERVED_WORD_PREFIX_OPTION, RESERVED_WORD_PREFIX_DESC, this.reservedWordPrefix);
        addSwitch(OPTION_USE_STRUCT_MODEL, OPTION_USE_STRUCT_MODEL_DESC, this.isUseStructModel);
        addOption(VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_OPTION,
                VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_DESC,
                Boolean.toString(this.variableNameFirstCharacterUppercase));

        supportingFiles.add(new SupportingFile("helpers-header.mustache", "model", modelNamePrefix + "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-source.mustache", "model", modelNamePrefix + "Helpers.cpp"));
        supportingFiles.add(new SupportingFile("main-api-server.mustache", "", modelNamePrefix + "main-api-server.cpp"));
        supportingFiles.add(new SupportingFile("cmake.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("int", "char", "bool", "long", "float", "double", "int32_t", "int64_t"));

        typeMapping = new HashMap<>();
        typeMapping.put("date", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int32_t");
        typeMapping.put("long", "int64_t");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::vector");
        typeMapping.put("map", "std::map");
        typeMapping.put("set", "std::vector");
        typeMapping.put("file", "std::string");
        typeMapping.put("object", "Object");
        typeMapping.put("binary", "std::string");
        typeMapping.put("number", "double");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");
        typeMapping.put("ByteArray", "std::string");

        super.importMapping = new HashMap<>();
        importMapping.put("std::vector", "#include <vector>");
        importMapping.put("std::map", "#include <map>");
        importMapping.put("std::string", "#include <string>");
        importMapping.put("Object", "#include \"Object.h\"");
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(HELPERS_PACKAGE_NAME)) {
            helpersPackage = (String) additionalProperties.get(HELPERS_PACKAGE_NAME);
        }
        if (additionalProperties.containsKey("modelNamePrefix")) {
            additionalProperties().put("prefix", modelNamePrefix);
            supportingFiles.clear();
            supportingFiles.add(new SupportingFile("helpers-header.mustache", "model", modelNamePrefix + "Helpers.h"));
            supportingFiles.add(new SupportingFile("helpers-source.mustache", "model", modelNamePrefix + "Helpers.cpp"));
            supportingFiles.add(new SupportingFile("main-api-server.mustache", "", modelNamePrefix + "main-api-server.cpp"));
            supportingFiles.add(new SupportingFile("cmake.mustache", "", "CMakeLists.txt"));
            supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        }
        if (additionalProperties.containsKey(RESERVED_WORD_PREFIX_OPTION)) {
            reservedWordPrefix = (String) additionalProperties.get(RESERVED_WORD_PREFIX_OPTION);
        }

        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
        additionalProperties.put("helpersNamespaceDeclarations", helpersPackage.split("\\."));
        additionalProperties.put("helpersNamespace", helpersPackage.replaceAll("\\.", "::"));
        additionalProperties.put(RESERVED_WORD_PREFIX_OPTION, reservedWordPrefix);

        if (additionalProperties.containsKey(OPTIONAL_EXTERNAL_LIB)) {
            setAddExternalLibs(convertPropertyToBooleanAndWriteBack(OPTIONAL_EXTERNAL_LIB));
        } else {
            additionalProperties.put(OPTIONAL_EXTERNAL_LIB, isAddExternalLibs);
        }

        setupModelTemplate();
    }

    private void setupModelTemplate() {
        if (additionalProperties.containsKey(OPTION_USE_STRUCT_MODEL))
            isUseStructModel = convertPropertyToBooleanAndWriteBack(OPTION_USE_STRUCT_MODEL);

        if (isUseStructModel) {
            LOGGER.info("Using struct-based model template");
            modelTemplateFiles.put("model-struct-header.mustache", ".h");
            modelTemplateFiles.put("model-struct-source.mustache", ".cpp");
        } else {
            LOGGER.info("Using get/set-based model template");
            modelTemplateFiles.put("model-header.mustache", ".h");
            modelTemplateFiles.put("model-source.mustache", ".cpp");
        }
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        } else {
            return "#include \"" + name + ".h\"";
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

        if(!codegenModel.isEnum
                && codegenModel.anyOf.size()>1
                && codegenModel.anyOf.contains("std::string")
                && !codegenModel.anyOf.contains("AnyType")
                && codegenModel.interfaces.size()==1
        ){
            codegenModel.vendorExtensions.put("x-is-string-enum-container",true);
        }
        return codegenModel;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse apiResponse = findMethodResponse(operation.getResponses());

            if (apiResponse != null) {
                Schema response = ModelUtils.getSchemaFromResponse(apiResponse);
                if (response != null) {
                    CodegenProperty cm = fromProperty("response", response);
                    op.vendorExtensions.put("x-codegen-response", cm);
                    if ("HttpContent".equals(cm.dataType)) {
                        op.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                    }
                }
            }
        }

        String pathForPistache = path.replaceAll("\\{(.*?)}", ":$1");
        op.vendorExtensions.put("x-codegen-pistache-path", pathForPistache);

        return op;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        String classname = (String) operations.get("classname");
        operations.put("classnameSnakeUpperCase", underscore(classname).toUpperCase(Locale.ROOT));
        operations.put("classnameSnakeLowerCase", underscore(classname).toLowerCase(Locale.ROOT));
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            boolean consumeJson = false;
            boolean isParsingSupported = true;
            if (op.bodyParam != null) {
                if (op.bodyParam.vendorExtensions == null) {
                    op.bodyParam.vendorExtensions = new HashMap<>();
                }

                boolean isStringOrDate = op.bodyParam.isString || op.bodyParam.isDate;
                op.bodyParam.vendorExtensions.put("x-codegen-pistache-is-string-or-date", isStringOrDate);
            }
            if (op.consumes != null) {
                for (Map<String, String> consume : op.consumes) {
                    if (consume.get("mediaType") != null && consume.get("mediaType").equals("application/json")) {
                        consumeJson = true;
                    }
                }
            }

            op.httpMethod = op.httpMethod.substring(0, 1).toUpperCase(Locale.ROOT) + op.httpMethod.substring(1).toLowerCase(Locale.ROOT);

            for (CodegenParameter param : op.allParams) {
                if (param.isFormParam) isParsingSupported = false;
                if (param.isFile) isParsingSupported = false;
                if (param.isCookieParam) isParsingSupported = false;

                //TODO: This changes the info about the real type but it is needed to parse the header params
                if (param.isHeaderParam) {
                    param.dataType = "std::optional<Pistache::Http::Header::Raw>";
                    param.baseType = "std::optional<Pistache::Http::Header::Raw>";
                } else if (param.isQueryParam) {
                    if (param.isPrimitiveType) {
                        param.dataType = "std::optional<" + param.dataType + ">";
                    } else {
                        param.dataType = "std::optional<" + param.dataType + ">";
                        param.baseType = "std::optional<" + param.baseType + ">";
                    }
                }
            }

            if (op.vendorExtensions == null) {
                op.vendorExtensions = new HashMap<>();
            }
            op.vendorExtensions.put("x-codegen-pistache-consumes-json", consumeJson);
            op.vendorExtensions.put("x-codegen-pistache-is-parsing-supported", isParsingSupported);

            // Check if any one of the operations needs a model, then at API file level, at least one model has to be included.
            for (String hdr : op.imports) {
                if (importMapping.containsKey(hdr)) {
                    continue;
                }
                operations.put("hasModelImport", true);
            }
        }

        return objs;
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if (templateName.endsWith("impl-header.mustache")) {
            int ix = result.lastIndexOf(File.separatorChar);
            result = result.substring(0, ix) + result.substring(ix, result.length() - 2) + "Impl.h";
            result = result.replace(apiFileFolder(), implFileFolder());
        } else if (templateName.endsWith("impl-source.mustache")) {
            int ix = result.lastIndexOf(File.separatorChar);
            result = result.substring(0, ix) + result.substring(ix, result.length() - 4) + "Impl.cpp";
            result = result.replace(apiFileFolder(), implFileFolder());
        }
        return result;
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
     * `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">";
        }
        if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
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

        return openAPIType;
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
            String inner = getSchemaType(getAdditionalProperties(p));
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

    private String implFileFolder() {
        return (outputFolder + "/" + implFolder).replace("/", File.separator);
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
            if (languageSpecificPrimitives.contains(type))
                return toModelName(type);
        } else
            type = openAPIType;
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
