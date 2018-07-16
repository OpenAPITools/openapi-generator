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

import static com.google.common.base.Strings.isNullOrEmpty;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.ModelUtils;

import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class CppRestSdkClientCodegen extends AbstractCppCodegen {

    public static final String DECLSPEC = "declspec";
    public static final String DEFAULT_INCLUDE = "defaultInclude";
    public static final String GENERATE_GMOCKS_FOR_APIS = "generateGMocksForApis";
    public static final String SUPPORTING_FILES_DIRECTORY = "supportingFilesDirectory";
    public static final String SUPPORTING_FILES_NAMESPACE = "supportingFilesNamespace";

    protected String packageVersion = "1.0.0";
    protected String declspec = "";
    protected String defaultInclude = "";
    protected String supportingFilesDirectory = "";
    protected String supportingFilesNamespace = "";

    private final Set<String> parentModels = new HashSet<>();
    private final Multimap<String, CodegenModel> childrenByParent = ArrayListMultimap.create();

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator. This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "cpp-restsdk";
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with
     * help tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a C++ API client with C++ REST SDK (https://github.com/Microsoft/cpprestsdk).";
    }

    public CppRestSdkClientCodegen() {
        super();

        apiPackage = "org.openapitools.client.api";
        modelPackage = "org.openapitools.client.model";

        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");

        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "cpp-rest-sdk-client";

        cliOptions.clear();

        // CLI options
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
        addOption(SUPPORTING_FILES_DIRECTORY,
                "The directory where supporting files should be loaded from",
                this.supportingFilesDirectory);
        addOption(SUPPORTING_FILES_NAMESPACE,
                "The namespace used in supporting files",
                this.supportingFilesNamespace);

        supportingFiles.add(new SupportingFile("modelbase-header.mustache", "", "ModelBase.h"));
        supportingFiles.add(new SupportingFile("modelbase-source.mustache", "", "ModelBase.cpp"));
        supportingFiles.add(new SupportingFile("object-header.mustache", "", "Object.h"));
        supportingFiles.add(new SupportingFile("object-source.mustache", "", "Object.cpp"));
        supportingFiles.add(new SupportingFile("apiclient-header.mustache", "", "ApiClient.h"));
        supportingFiles.add(new SupportingFile("apiclient-source.mustache", "", "ApiClient.cpp"));
        supportingFiles.add(new SupportingFile("apiconfiguration-header.mustache", "", "ApiConfiguration.h"));
        supportingFiles.add(new SupportingFile("apiconfiguration-source.mustache", "", "ApiConfiguration.cpp"));
        supportingFiles.add(new SupportingFile("apiexception-header.mustache", "", "ApiException.h"));
        supportingFiles.add(new SupportingFile("apiexception-source.mustache", "", "ApiException.cpp"));
        supportingFiles.add(new SupportingFile("ihttpbody-header.mustache", "", "IHttpBody.h"));
        supportingFiles.add(new SupportingFile("jsonbody-header.mustache", "", "JsonBody.h"));
        supportingFiles.add(new SupportingFile("jsonbody-source.mustache", "", "JsonBody.cpp"));
        supportingFiles.add(new SupportingFile("httpcontent-header.mustache", "", "HttpContent.h"));
        supportingFiles.add(new SupportingFile("httpcontent-source.mustache", "", "HttpContent.cpp"));
        supportingFiles.add(new SupportingFile("multipart-header.mustache", "", "MultipartFormData.h"));
        supportingFiles.add(new SupportingFile("multipart-source.mustache", "", "MultipartFormData.cpp"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("cmake-lists.mustache", "", "CMakeLists.txt"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("int", "char", "bool", "long", "float", "double", "int32_t", "int64_t"));

        typeMapping = new HashMap<String, String>();
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
        typeMapping.put("binary", "std::string");
        typeMapping.put("number", "double");
        typeMapping.put("UUID", "utility::string_t");
        typeMapping.put("ByteArray", "utility::string_t");

        super.importMapping = new HashMap<String, String>();
        updateImportMapping();
    }

    private String supportingFilePath(String filename) {
        if(!supportingFilesDirectory.isEmpty()) {
            return Paths.get(supportingFilesDirectory, filename).toString();
        }
        return filename;
    }

    private void updateImportMapping() {
        importMapping.put("std::vector", globalInclude("vector"));
        importMapping.put("std::map", globalInclude("map"));
        importMapping.put("std::string", globalInclude("string"));
        importMapping.put("utility::string_t", globalInclude("cpprest/details/basic_types.h"));
        importMapping.put("utility::datetime", globalInclude("cpprest/details/basic_types.h"));
        importMapping.put("HttpContent", quoteInclude(supportingFilePath("HttpContent.h")));
        importMapping.put("ModelBase", quoteInclude(supportingFilePath("ModelBase.h")));
        importMapping.put("Object", quoteInclude(supportingFilePath("Object.h")));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(DECLSPEC)) {
            declspec = additionalProperties.get(DECLSPEC).toString();
        }

        if (additionalProperties.containsKey(DEFAULT_INCLUDE)) {
            defaultInclude = additionalProperties.get(DEFAULT_INCLUDE).toString();
        }

        if (convertPropertyToBoolean(GENERATE_GMOCKS_FOR_APIS)) {
            apiTemplateFiles.put("api-gmock.mustache", "GMock.h");
            additionalProperties.put("gmockApis", "true");
        }

        if (additionalProperties.containsKey(SUPPORTING_FILES_DIRECTORY)) {
            supportingFilesDirectory = additionalProperties.get(SUPPORTING_FILES_DIRECTORY).toString();

        }
        if (additionalProperties.containsKey(SUPPORTING_FILES_NAMESPACE)) {
            supportingFilesNamespace = additionalProperties.get(SUPPORTING_FILES_NAMESPACE).toString();
        }

        // Ensure that the supportingFilesNamespace always ends with "::"
        if(!supportingFilesNamespace.endsWith("::")) {
            supportingFilesNamespace += "::";
        }

        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("modelHeaderGuardPrefix", modelPackage.replaceAll("\\.", "_").toUpperCase());
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiHeaderGuardPrefix", apiPackage.replaceAll("\\.", "_").toUpperCase());
        additionalProperties.put("declspec", declspec);
        additionalProperties.put("defaultInclude", defaultInclude);
        additionalProperties.put("supportingFilesNamespace", supportingFilesNamespace);
        // Intentionally omitting:
        //  - supportingFilesDirectory
        // These have context-dependant defaults in the individual templates
    }

    /**
     * Location to write model files. You can use the modelPackage() as defined
     * when the class is instantiated
     */
    @Override
    public String modelFileFolder() {
        return outputFolder + "/model";
    }

    /**
     * Location to write api files. You can use the apiPackage() as defined when
     * the class is instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + "/api";
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        } else {
            // Use relative imports unless this model is explicitly import-mapped.
            return quoteInclude(Paths.get("..", "model", sanitizeName(name) + ".h").toString());
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema model, Map<String, Schema> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);

        Set<String> oldImports = codegenModel.imports;
        codegenModel.imports = new HashSet<String>();
        for (String imp : oldImports) {
            String newImp = toModelImport(imp);
            if (!newImp.isEmpty()) {
                codegenModel.imports.add(newImp);
            }
        }

        return codegenModel;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation,
                                          Map<String, Schema> schema, OpenAPI openAPI) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, schema, openAPI);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse methodResponse = findMethodResponse(operation.getResponses());

            if (methodResponse != null) {
                Schema response = ModelUtils.getSchemaFromResponse(methodResponse);
                if (response != null) {
                    CodegenProperty cm = fromProperty("response", response);
                    op.vendorExtensions.put("x-codegen-response", cm);
                    if ("HttpContent".equals(cm.dataType)) {
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

    protected boolean isFileSchema(CodegenProperty property) {
        return property.baseType.equals("HttpContent");
    }

    @Override
    public String toModelFilename(String name) {
        return sanitizeName(initialCaps(name));
    }

    @Override
    public String toApiFilename(String name) {
        return sanitizeName(initialCaps(name) + "Api");
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
            Schema inner = (Schema) p.getAdditionalProperties();
            return getSchemaType(p) + "<utility::string_t, " + getTypeDeclaration(inner) + ">";
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
            String inner = getSchemaType((Schema) p.getAdditionalProperties());
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
        }
        return "nullptr";
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        boolean isPrimitiveType = parameter.isPrimitiveType == Boolean.TRUE;
        boolean isListContainer = parameter.isListContainer == Boolean.TRUE;
        boolean isString = parameter.isString == Boolean.TRUE;

        if (!isPrimitiveType && !isListContainer && !isString && !parameter.dataType.startsWith("std::shared_ptr")) {
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
        for (final CodegenProperty childSchema : child.vars) {
            childPropertiesByName.put(childSchema.name, childSchema);
        }
        for (final CodegenProperty parentSchema : parent.vars) {
            final CodegenProperty duplicatedByParent = childPropertiesByName.get(parentSchema.name);
            if (duplicatedByParent != null) {
                duplicatedByParent.isInherited = true;
            }
        }
    }

}
