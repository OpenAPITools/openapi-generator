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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.*;
import java.util.Map.Entry;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class CppRestbedServerCodegen extends AbstractCppCodegen {

    public static final String DECLSPEC = "declspec";
    public static final String DEFAULT_INCLUDE = "defaultInclude";
    private static final String OPEN_API_PATH_PARAM_PATTERN = "^\\{(.*)\\}$";
    private static final String X_CODEGEN_OTHER_METHODS = "x-codegen-other-methods";

    protected String packageVersion = "1.0.0";
    protected String declspec = "";
    protected String defaultInclude = "";

    public CppRestbedServerCodegen() {
        super();

        // TODO: cpp-restbed-server maintainer review
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

        apiPackage = "org.openapitools.server.api";
        modelPackage = "org.openapitools.server.model";

        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");

        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "cpp-restbed-server";

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
        addOption(RESERVED_WORD_PREFIX_OPTION,
                RESERVED_WORD_PREFIX_DESC,
                this.reservedWordPrefix);

        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("model-helpers-header.mustache", modelFileFolder(), "helpers.h"));
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("FindRestbedAndBoost.cmake", "", "FindRestbedAndBoost.cmake"));

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
        typeMapping.put("set", "std::set");
        typeMapping.put("file", "std::string");
        typeMapping.put("object", "std::string");
        typeMapping.put("binary", "restbed::Bytes");
        typeMapping.put("number", "double");
        typeMapping.put("decimal", "std::string");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");
        typeMapping.put("ByteArray", "std::string");

        super.importMapping = new HashMap<>();
        importMapping.put("std::vector", "#include <vector>");
        importMapping.put("std::map", "#include <map>");
        importMapping.put("std::set", "#include <set>");
        importMapping.put("file", "#include <string>");
        importMapping.put("std::string", "#include <string>");
        importMapping.put("restbed::Bytes", "#include <corvusoft/restbed/byte.hpp>");
    }

    @Override
    public Map<String, ModelsMap> updateAllModels(Map<String, ModelsMap> objs) {
        // Index all CodegenModels by model name.
        Map<String, CodegenModel> allModels = getAllModels(objs);

        // Clean interfaces of ambiguity
        for (Entry<String, CodegenModel> cm : allModels.entrySet()) {
            if (cm.getValue().getInterfaces() != null && !cm.getValue().getInterfaces().isEmpty()) {
                List<String> newIntf = new ArrayList<>(cm.getValue().getInterfaces());

                for (String intf : allModels.get(cm.getKey()).getInterfaces()) {
                    if (allModels.get(intf).getInterfaces() != null && !allModels.get(intf).getInterfaces().isEmpty()) {
                        for (String intfInner : allModels.get(intf).getInterfaces()) {
                            newIntf.remove(intfInner);
                        }
                    }
                }
                cm.getValue().setInterfaces(newIntf);
            }
        }

        objs = super.updateAllModels(objs);
        return objs;
    }

    /**
     * Camelize the method name of the getter and setter, but keep underscores at the front
     *
     * @param name string to be camelized
     * @return Camelized string
     */
    @Override
    public String getterAndSetterCapitalize(String name) {
        if (name == null || name.length() == 0) {
            return name;
        }

        name = toVarName(name);

        if (name.startsWith("_")) {
            return "_" + camelize(name);
        }

        return camelize(name);
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator. This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "cpp-restbed-server";
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with
     * help tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a C++ API Server with Restbed (https://github.com/Corvusoft/restbed).";
    }

    @Override
    public String getTypeDeclaration(String str) {
        return toModelName(str);
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

        if (additionalProperties.containsKey(RESERVED_WORD_PREFIX_OPTION)) {
            reservedWordPrefix = additionalProperties.get(RESERVED_WORD_PREFIX_OPTION).toString();
        }

        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
        additionalProperties.put("declspec", declspec);
        additionalProperties.put("defaultInclude", defaultInclude);
        additionalProperties.put(RESERVED_WORD_PREFIX_OPTION, reservedWordPrefix);
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
        return codegenModel;
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }


    private String capitalizeFirstChar(String str) {
        if (str.length() > 1) {
            return Character.toUpperCase(str.charAt(0)) + str.substring(1);
        } else {
            return str.toUpperCase(Locale.ENGLISH);
        }
    }

    private String convertPathSegmentToResourceNamePart(String pathSegment) {
        String convertedSegnemt = pathSegment;
        if (pathSegment.matches(OPEN_API_PATH_PARAM_PATTERN)) {
            convertedSegnemt = pathSegment.substring(1, pathSegment.length() - 1);
        }
        return capitalizeFirstChar(sanitizeName(convertedSegnemt));
    }

    private String convertPathParamPattern(String pathSegment) {
        if (pathSegment.matches(OPEN_API_PATH_PARAM_PATTERN)) {
            String pattern = pathSegment.substring(0, pathSegment.length() - 1);
            pattern += ": .*}";
            return pattern;
        }
        return pathSegment;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();
        List<CodegenOperation> newOpList = new ArrayList<>();

        for (CodegenOperation op : operationList) {
            String[] pathSegments = op.path.split("/", -1);
            String resourceNameCamelCase = "";
            StringJoiner joiner = new StringJoiner("/");
            for (String pathSegment : pathSegments) {
                resourceNameCamelCase += convertPathSegmentToResourceNamePart(pathSegment);
                String convertedPathSegment = convertPathParamPattern(pathSegment);
                joiner.add(convertedPathSegment);
                    }

            op.path = joiner.toString();
            op.vendorExtensions.put("x-codegen-resource-name", resourceNameCamelCase);


            CodegenOperation op1 = newOpList.stream()
                    .filter(opInList -> opInList.path.equals(op.path))
                    .findAny()
                    .orElse(null);

            if (op1 != null) {
                List<CodegenOperation> currentOtherMethodList = getCodegenXCodegenOtherMethodsOperations(op1);
                        op.operationIdCamelCase = op1.operationIdCamelCase;
                        currentOtherMethodList.add(op);
                        op1.vendorExtensions.put(X_CODEGEN_OTHER_METHODS, currentOtherMethodList);
                    }
            else {
                newOpList.add(op);
            }
        }
        operations.setOperation(newOpList);
        return objs;
    }

    private List<CodegenOperation> getCodegenXCodegenOtherMethodsOperations(CodegenOperation newOperation) {
        List<CodegenOperation> currentOtherMethodList = (List<CodegenOperation>) newOperation.vendorExtensions.get(X_CODEGEN_OTHER_METHODS);
        if (currentOtherMethodList == null) {
            currentOtherMethodList = new ArrayList<>();
        }
        return currentOtherMethodList;
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
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return getSchemaType(p) + "<std::string, " + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isByteArraySchema(p)) {
            return "std::string";
        } else if (ModelUtils.isFileSchema(p)) {
            return "std::string";
        } else if (ModelUtils.isStringSchema(p)
                || ModelUtils.isDateSchema(p)
                || ModelUtils.isDateTimeSchema(p)
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
        } else if (ModelUtils.isFileSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "std::string{}";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (ModelUtils.isFloatSchema(p)) { // float
                if (p.getDefault() != null) {
                    return p.getDefault().toString() + "f";
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
        } else if (ModelUtils.isSet(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            return "std::set<" + inner + ">()";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            return "std::vector<" + inner + ">()";
        } else if (ModelUtils.isModel(p)) {
            String modelName = getTypeDeclaration(p);
            return modelName + "{}";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            return toModelName(ModelUtils.getSimpleRef(p.get$ref())) + "{}";
        }

        return "nullptr";
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        boolean isPrimitiveType = parameter.isPrimitiveType == Boolean.TRUE;
        boolean isArray = parameter.isArray == Boolean.TRUE;
        boolean isString = parameter.isString == Boolean.TRUE;

        if (!isPrimitiveType && !isArray && !isString && !parameter.dataType.startsWith("std::shared_ptr")) {
            parameter.defaultValue =  parameter.dataType + "{}";
        }
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
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        // Remove prefix added by DefaultCodegen
        String originalDefaultValue = var.defaultValue;
        super.updateCodegenPropertyEnum(var);
        var.defaultValue = originalDefaultValue;
    }
}
