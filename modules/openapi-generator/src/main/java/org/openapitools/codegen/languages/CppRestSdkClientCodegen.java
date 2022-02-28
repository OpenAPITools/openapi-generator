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

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.*;

import static com.google.common.base.Strings.isNullOrEmpty;

public class CppRestSdkClientCodegen extends AbstractCppCodegen {

    public static final String DECLSPEC = "declspec";
    public static final String DEFAULT_INCLUDE = "defaultInclude";
    public static final String GENERATE_GMOCKS_FOR_APIS = "generateGMocksForApis";
    public static final String DEFAULT_PACKAGE_NAME = "CppRestOpenAPIClient";

    protected String packageName = "";
    protected String packageVersion = "1.0.0";
    protected String declspec = "";
    protected String defaultInclude = "";
    protected String apiDirName = "api";
    protected String modelDirName = "model";

    private final Set<String> parentModels = new HashSet<>();
    private final Multimap<String, CodegenModel> childrenByParent = ArrayListMultimap.create();

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator. This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "cpp-restsdk";
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with
     * help tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a C++ API client with C++ REST SDK (https://github.com/Microsoft/cpprestsdk).";
    }

    public CppRestSdkClientCodegen() {
        super();

        // TODO: cpp-restsdk maintainer review
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.ApiKey
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        apiPackage = "org.openapitools.client.api";
        modelPackage = "org.openapitools.client.model";

        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");

        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "cpp-rest-sdk-client";

        cliOptions.clear();

        // CLI options
        addOption(CodegenConstants.PACKAGE_NAME, "C++ package (library) name.", DEFAULT_PACKAGE_NAME);
        addOption(CodegenConstants.MODEL_PACKAGE, "C++ namespace for models (convention: name.space.model).",
                this.modelPackage);
        addOption(CodegenConstants.API_PACKAGE, "C++ namespace for apis (convention: name.space.api).",
                this.apiPackage);
        addOption(CodegenConstants.PACKAGE_VERSION, "C++ package version.", this.packageVersion);
        addOption(DECLSPEC, "C++ preprocessor to place before the class name for handling dllexport/dllimport.",
                this.declspec);
        addOption(DEFAULT_INCLUDE,
                "The default include statement that should be placed in all headers for including things like the declspec (convention: #include \"Commons.h\" ",
                this.defaultInclude);
        addOption(GENERATE_GMOCKS_FOR_APIS,
                "Generate Google Mock classes for APIs.",
                null);
        addOption(RESERVED_WORD_PREFIX_OPTION,
                RESERVED_WORD_PREFIX_DESC,
                this.reservedWordPrefix);
        addOption(VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_OPTION,
                VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_DESC,
                Boolean.toString(this.variableNameFirstCharacterUppercase));

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("int", "char", "bool", "long", "float", "double", "int32_t", "int64_t"));

        typeMapping = new HashMap<>();
        typeMapping.put("date", "utility::datetime");
        typeMapping.put("DateTime", "utility::datetime");
        typeMapping.put("string", "utility::string_t");
        typeMapping.put("integer", "int32_t");
        typeMapping.put("long", "int64_t");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::vector");
        typeMapping.put("map", "std::map");
        typeMapping.put("file", "HttpContent");
        typeMapping.put("object", "Object");
        typeMapping.put("binary", "HttpContent");
        typeMapping.put("number", "double");
        typeMapping.put("UUID", "utility::string_t");
        typeMapping.put("URI", "utility::string_t");
        typeMapping.put("ByteArray", "utility::string_t");

        super.importMapping = new HashMap<>();
        importMapping.put("std::vector", "#include <vector>");
        importMapping.put("std::map", "#include <map>");
        importMapping.put("std::string", "#include <string>");
        importMapping.put("HttpContent", "#include \"HttpContent.h\"");
        importMapping.put("Object", "#include \"Object.h\"");
        importMapping.put("utility::string_t", "#include <cpprest/details/basic_types.h>");
        importMapping.put("utility::datetime", "#include <cpprest/details/basic_types.h>");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        packageName = (String) additionalProperties.getOrDefault(CodegenConstants.PACKAGE_NAME, DEFAULT_PACKAGE_NAME);

        if (additionalProperties.containsKey(DECLSPEC)) {
            declspec = additionalProperties.get(DECLSPEC).toString();
        }

        if (additionalProperties.containsKey(DEFAULT_INCLUDE)) {
            defaultInclude = additionalProperties.get(DEFAULT_INCLUDE).toString();
        }

        if (additionalProperties.containsKey(RESERVED_WORD_PREFIX_OPTION)) {
            reservedWordPrefix = (String) additionalProperties.get(RESERVED_WORD_PREFIX_OPTION);
        }

        if (convertPropertyToBoolean(GENERATE_GMOCKS_FOR_APIS)) {
            apiTemplateFiles.put("api-gmock.mustache", "GMock.h");
            additionalProperties.put("gmockApis", "true");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("modelHeaderGuardPrefix", modelPackage.replaceAll("\\.", "_").toUpperCase(Locale.ROOT));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiHeaderGuardPrefix", apiPackage.replaceAll("\\.", "_").toUpperCase(Locale.ROOT));
        additionalProperties.put("declspec", declspec);
        additionalProperties.put("defaultInclude", defaultInclude);
        additionalProperties.put(RESERVED_WORD_PREFIX_OPTION, reservedWordPrefix);

        importMapping.put("HttpContent", "#include \"" + packageName + "/" + "HttpContent.h\"");
        importMapping.put("Object", "#include \"" + packageName + "/" + "Object.h\"");

        supportingFiles.add(new SupportingFile("modelbase-header.mustache", getHeaderFolder(), "ModelBase.h"));
        supportingFiles.add(new SupportingFile("modelbase-source.mustache", getSourceFolder(), "ModelBase.cpp"));
        supportingFiles.add(new SupportingFile("object-header.mustache", getHeaderFolder(), "Object.h"));
        supportingFiles.add(new SupportingFile("object-source.mustache", getSourceFolder(), "Object.cpp"));
        supportingFiles.add(new SupportingFile("apiclient-header.mustache", getHeaderFolder(), "ApiClient.h"));
        supportingFiles.add(new SupportingFile("apiclient-source.mustache", getSourceFolder(), "ApiClient.cpp"));
        supportingFiles.add(new SupportingFile("apiconfiguration-header.mustache", getHeaderFolder(), "ApiConfiguration.h"));
        supportingFiles.add(new SupportingFile("apiconfiguration-source.mustache", getSourceFolder(), "ApiConfiguration.cpp"));
        supportingFiles.add(new SupportingFile("apiexception-header.mustache", getHeaderFolder(), "ApiException.h"));
        supportingFiles.add(new SupportingFile("apiexception-source.mustache", getSourceFolder(), "ApiException.cpp"));
        supportingFiles.add(new SupportingFile("ihttpbody-header.mustache", getHeaderFolder(), "IHttpBody.h"));
        supportingFiles.add(new SupportingFile("jsonbody-header.mustache", getHeaderFolder(), "JsonBody.h"));
        supportingFiles.add(new SupportingFile("jsonbody-source.mustache", getSourceFolder(), "JsonBody.cpp"));
        supportingFiles.add(new SupportingFile("httpcontent-header.mustache", getHeaderFolder(), "HttpContent.h"));
        supportingFiles.add(new SupportingFile("httpcontent-source.mustache", getSourceFolder(), "HttpContent.cpp"));
        supportingFiles.add(new SupportingFile("multipart-header.mustache", getHeaderFolder(), "MultipartFormData.h"));
        supportingFiles.add(new SupportingFile("multipart-source.mustache", getSourceFolder(), "MultipartFormData.cpp"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("cmake-lists.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("cmake-config.mustache", "", "Config.cmake.in"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    protected String getHeaderFolder() {
        return "include/" + packageName;
    }

    protected String getSourceFolder() {
        return "src";
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        String targetOutDir = suffix.equals(".h") ? getHeaderFolder() : getSourceFolder();
        return outputFolder + "/" + targetOutDir + "/" + apiDirName + "/" + toApiFilename(tag) + suffix;
    }

    @Override
    public String modelFilename(String templateName, String modelName) {
        String suffix = modelTemplateFiles().get(templateName);
        String targetOutDir = suffix.equals(".h") ? getHeaderFolder() : getSourceFolder();
        return outputFolder + "/" + targetOutDir + "/" + modelDirName + "/" + toModelFilename(modelName) + suffix;
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        } else {
            return "#include \"" + packageName + "/" + modelDirName + "/" + toModelFilename(name) + ".h\"";
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

        return codegenModel;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse methodResponse = findMethodResponse(operation.getResponses());

            if (methodResponse != null) {
                Schema response = ModelUtils.getSchemaFromResponse(methodResponse);
                response = ModelUtils.unaliasSchema(this.openAPI, response, importMapping);
                if (response != null) {
                    CodegenProperty cm = fromProperty("response", response);
                    op.vendorExtensions.put("x-codegen-response", cm);
                    if ("std::shared_ptr<HttpContent>".equals(cm.dataType)) {
                        op.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                    }
                }
            }
        }

        return op;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if (isFileSchema(property)) {
            property.vendorExtensions.put("x-codegen-file", true);
        }

        if (!isNullOrEmpty(model.parent)) {
            parentModels.add(model.parent);
            if (!childrenByParent.containsEntry(model.parent, model)) {
                childrenByParent.put(model.parent, model);
            }
        }
    }

    // override with any special post-processing
    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            for (String hdr : op.imports) {
                if (importMapping.containsKey(hdr)) {
                    continue;
                }
                operations.put("hasModelImport", true);
                break;
            }
        }
        return objs;
    }

    protected boolean isFileSchema(CodegenProperty property) {
        return property.baseType.equals("HttpContent");
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
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
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "<utility::string_t, " + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isFileSchema(p) || ModelUtils.isBinarySchema(p)) {
            return "std::shared_ptr<" + openAPIType + ">";
        } else if (ModelUtils.isStringSchema(p)
                || ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p)
                || ModelUtils.isFileSchema(p) || ModelUtils.isUUIDSchema(p)
                || languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        }

        return "std::shared_ptr<" + openAPIType + ">";
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            return "false";
        } else if (ModelUtils.isDateSchema(p)) {
            return "utility::datetime()";
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return "utility::datetime()";
        } else if (ModelUtils.isNumberSchema(p)) {
            if (ModelUtils.isFloatSchema(p)) {
                return "0.0f";
            }
            return "0.0";
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (ModelUtils.isLongSchema(p)) {
                return "0L";
            }
            return "0";
        } else if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(getAdditionalProperties(p));
            return "std::map<utility::string_t, " + inner + ">()";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            if (!languageSpecificPrimitives.contains(inner)) {
                inner = "std::shared_ptr<" + inner + ">";
            }
            return "std::vector<" + inner + ">()";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            return "new " + toModelName(ModelUtils.getSimpleRef(p.get$ref())) + "()";
        } else if (ModelUtils.isStringSchema(p)) {
            return "utility::conversions::to_string_t(\"\")";
        } else if (isFreeFormObject(p)) {
            return "new Object()";
        }

        return "nullptr";
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        boolean isPrimitiveType = parameter.isPrimitiveType == Boolean.TRUE;
        boolean isArray = parameter.isArray == Boolean.TRUE;
        boolean isMap = parameter.isMap == Boolean.TRUE;
        boolean isString = parameter.isString == Boolean.TRUE;

        if (!isPrimitiveType && !isArray && !isMap && !isString && !parameter.dataType.startsWith("std::shared_ptr")) {
            parameter.dataType = "std::shared_ptr<" + parameter.dataType + ">";
        }
    }

    /**
     * Optional - OpenAPI type conversion. This is used to map OpenAPI types in
     * a `Schema` into either language specific types via `typeMapping` or
     * into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     * @see io.swagger.v3.oas.models.media.Schema
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
    public Map<String, Object> postProcessAllModels(final Map<String, Object> models) {
        final Map<String, Object> processed = super.postProcessAllModels(models);
        postProcessParentModels(models);
        return processed;
    }

    private void postProcessParentModels(final Map<String, Object> models) {
        for (final String parent : parentModels) {
            final CodegenModel parentModel = ModelUtils.getModelByName(parent, models);
            final Collection<CodegenModel> childrenModels = childrenByParent.get(parent);
            for (final CodegenModel child : childrenModels) {
                processParentPropertiesInChildModel(parentModel, child);
            }
        }
    }

    /**
     * Sets the child property's isInherited flag to true if it is an inherited property
     */
    private void processParentPropertiesInChildModel(final CodegenModel parent, final CodegenModel child) {
        final Map<String, CodegenProperty> childPropertiesByName = new HashMap<>(child.vars.size());
        if (child != null && child.vars != null && !child.vars.isEmpty()) {
            for (final CodegenProperty childSchema : child.vars) {
                childPropertiesByName.put(childSchema.name, childSchema);
            }
        }

        if (parent != null && parent.vars != null && !parent.vars.isEmpty()) {
            for (final CodegenProperty parentSchema : parent.vars) {
                final CodegenProperty duplicatedByParent = childPropertiesByName.get(parentSchema.name);
                if (duplicatedByParent != null) {
                    duplicatedByParent.isInherited = true;
                }
            }
        }
    }

}
