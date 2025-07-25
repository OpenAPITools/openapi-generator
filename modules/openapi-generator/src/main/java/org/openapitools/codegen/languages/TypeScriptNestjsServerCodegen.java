/*
 * Copyright 2024 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import io.swagger.v3.oas.models.media.Schema;
import lombok.Getter;
import lombok.Setter;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.SemVer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.apache.commons.lang3.StringUtils.capitalize;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.*;

public class TypeScriptNestjsServerCodegen extends AbstractTypeScriptClientCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(TypeScriptNestjsServerCodegen.class);

    private static String CLASS_NAME_SUFFIX_PATTERN = "^[a-zA-Z0-9]*$";
    private static String FILE_NAME_SUFFIX_PATTERN = "^[a-zA-Z0-9.-]*$";

    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String TAGGED_UNIONS = "taggedUnions";
    public static final String NEST_VERSION = "nestVersion";
    public static final String API_SUFFIX = "apiSuffix";
    public static final String API_FILE_SUFFIX = "apiFileSuffix";
    public static final String MODEL_SUFFIX = "modelSuffix";
    public static final String MODEL_FILE_SUFFIX = "modelFileSuffix";
    public static final String FILE_NAMING = "fileNaming";
    public static final String STRING_ENUMS = "stringEnums";
    public static final String STRING_ENUMS_DESC = "Generate string enums instead of objects for enum values.";
    public static final String USE_SINGLE_REQUEST_PARAMETER = "useSingleRequestParameter";
    public static final String NODE_VERSION = "nodeVersion";
    public static final String TS_VERSION = "tsVersion";
    public static final String RXJS_VERSION = "rxjsVersion";

    protected String nestVersion = "10.0.0";
    @Getter
    @Setter
    protected String npmRepository = null;
    protected String apiSuffix = "Api";
    protected String apiFileSuffix = ".api";
    protected String modelSuffix = "";
    protected String modelFileSuffix = "";
    protected String fileNaming = "kebab-case";
    @Getter
    protected Boolean stringEnums = false;

    private boolean taggedUnions = false;

    private String nodeVersion = "22.17.0";
    private String rxJsVersion = "7.8.1";
    private String tsVersion = "5.7.3";

    public TypeScriptNestjsServerCodegen() {
        super();

        this.outputFolder = "generated-code/typescript-nestjs-server";

        supportsMultipleInheritance = true;

        modifyFeatureSet(features -> features
                .schemaSupportFeatures(EnumSet.of(
                        SchemaSupportFeature.Simple,
                        SchemaSupportFeature.oneOf
                ))
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        embeddedTemplateDir = templateDir = "typescript-nestjs-server";
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("controller.mustache", ".ts");
        apiTemplateFiles.put("api.mustache", ".ts");
        languageSpecificPrimitives.add("Blob");
        typeMapping.put("file", "Blob");
        apiPackage = "api";
        modelPackage = "models";

        reservedWords.addAll(Arrays.asList("from", "headers"));

        this.cliOptions.add(new CliOption(NPM_REPOSITORY,
                "Use this property to set an url your private npmRepo in the package.json"));
        this.cliOptions.add(CliOption.newBoolean(TAGGED_UNIONS,
                "Use discriminators to create tagged unions instead of extending interfaces.",
                this.taggedUnions));
        this.cliOptions.add(new CliOption(NEST_VERSION, "The version of Nestjs. (At least 10.0.0)").defaultValue(this.nestVersion));
        this.cliOptions.add(new CliOption(API_SUFFIX, "The suffix of the generated API class").defaultValue(this.apiSuffix));
        this.cliOptions.add(new CliOption(API_FILE_SUFFIX, "The suffix of the file of the generated API class (api<suffix>.ts).").defaultValue(this.apiFileSuffix));
        this.cliOptions.add(new CliOption(MODEL_SUFFIX, "The suffix of the generated model."));
        this.cliOptions.add(new CliOption(MODEL_FILE_SUFFIX, "The suffix of the file of the generated model (model<suffix>.ts)."));
        this.cliOptions.add(new CliOption(FILE_NAMING, "Naming convention for the output files: 'camelCase', 'kebab-case'.").defaultValue(this.fileNaming));
        this.cliOptions.add(new CliOption(STRING_ENUMS, STRING_ENUMS_DESC).defaultValue(String.valueOf(this.stringEnums)));
        this.cliOptions.add(new CliOption(USE_SINGLE_REQUEST_PARAMETER, "Setting this property to true will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter.").defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(TS_VERSION, "The version of typescript compatible with Angular (see ngVersion option)."));
        this.cliOptions.add(new CliOption(RXJS_VERSION, "The version of RxJS compatible with Angular (see ngVersion option)."));
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        codegenModel.additionalPropertiesType = getTypeDeclaration(ModelUtils.getAdditionalProperties(schema));
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "typescript-nestjs-server";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript NestJS server stub.";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.add(new SupportingFile("module.mustache", "", "index.ts"));
        supportingFiles.add(
                new SupportingFile("models.mustache", modelPackage().replace('.', File.separatorChar), "index.ts"));
        supportingFiles
                .add(new SupportingFile("apis.mustache", apiPackage().replace('.', File.separatorChar), "index.ts"));
        supportingFiles.add(new SupportingFile("api-implementations.mustache", "", "api-implementations.ts"));
        supportingFiles.add(new SupportingFile("api.module.mustache", "", "api.module.ts"));
        supportingFiles.add(new SupportingFile("controllers.mustache", "controllers", "index.ts"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
        supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));

        // determine Nestjs version
        SemVer nestVersion;
        if (additionalProperties.containsKey(NEST_VERSION)) {
            nestVersion = new SemVer(additionalProperties.get(NEST_VERSION).toString());
        } else {
            nestVersion = new SemVer(this.nestVersion);
            LOGGER.info("generating code for Nestjs {} ...", nestVersion);
            LOGGER.info("  (you can select the nestjs version by setting the additionalProperty nestVersion)");
        }

        additionalProperties.put(NEST_VERSION, nestVersion);

        if (additionalProperties.containsKey(NPM_NAME)) {
            if(!additionalProperties.containsKey(NPM_VERSION)) {
                additionalProperties.put(NPM_VERSION, "0.0.0");
            }

            if (additionalProperties.containsKey(TS_VERSION)) {
                tsVersion = additionalProperties.get(TS_VERSION).toString();
            } else {
                additionalProperties.put(TS_VERSION, tsVersion);
            }

            if (additionalProperties.containsKey(RXJS_VERSION)) {
                rxJsVersion = additionalProperties.get(RXJS_VERSION).toString();
            } else {
                additionalProperties.put(RXJS_VERSION, rxJsVersion);
            }

            if (additionalProperties.containsKey(NODE_VERSION)) {
                nodeVersion = additionalProperties.get(NODE_VERSION).toString();
            } else {
                additionalProperties.put(NODE_VERSION, nodeVersion);
            }

            addNpmPackageGeneration();
        }

        if (additionalProperties.containsKey(STRING_ENUMS)) {
            setStringEnums(Boolean.parseBoolean(additionalProperties.get(STRING_ENUMS).toString()));
            additionalProperties.put("stringEnums", getStringEnums());
            if (getStringEnums()) {
                enumSuffix = "";
                classEnumSeparator = "";
            }
        }

        if (additionalProperties.containsKey(TAGGED_UNIONS)) {
            taggedUnions = Boolean.parseBoolean(additionalProperties.get(TAGGED_UNIONS).toString());
        }


        if (additionalProperties.containsKey(API_SUFFIX)) {
            apiSuffix = additionalProperties.get(API_SUFFIX).toString();
            validateClassSuffixArgument("Service", apiSuffix);
        }
        if (additionalProperties.containsKey(API_FILE_SUFFIX)) {
            apiFileSuffix = additionalProperties.get(API_FILE_SUFFIX).toString();
            validateFileSuffixArgument("Service", apiFileSuffix);
        }
        if (additionalProperties.containsKey(MODEL_SUFFIX)) {
            modelSuffix = additionalProperties.get(MODEL_SUFFIX).toString();
            validateClassSuffixArgument("Model", modelSuffix);
        }
        if (additionalProperties.containsKey(MODEL_FILE_SUFFIX)) {
            modelFileSuffix = additionalProperties.get(MODEL_FILE_SUFFIX).toString();
            validateFileSuffixArgument("Model", modelFileSuffix);
        }
        if (additionalProperties.containsKey(FILE_NAMING)) {
            this.setFileNaming(additionalProperties.get(FILE_NAMING).toString());
        }
    }

    private void addNpmPackageGeneration() {
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
    }

    public void setStringEnums(boolean value) {
        this.stringEnums = value;
    }

    @Override
    public boolean isDataTypeFile(final String dataType) {
        return "Blob".equals(dataType);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isFileSchema(p)) {
            return "Blob";
        } else {
            return super.getTypeDeclaration(p);
        }
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        if (isLanguagePrimitive(openAPIType) || isLanguageGenericType(openAPIType)) {
            return openAPIType;
        }
        applyLocalTypeMapping(openAPIType);
        return openAPIType;
    }

    private String applyLocalTypeMapping(String type) {
        if (typeMapping.containsKey(type)) {
            type = typeMapping.get(type);
        }
        return type;
    }

    private boolean isLanguagePrimitive(String type) {
        return languageSpecificPrimitives.contains(type);
    }

    private boolean isLanguageGenericType(String type) {
        for (String genericType : languageGenericTypes) {
            if (type.startsWith(genericType + "<")) {
                return true;
            }
        }
        return false;
    }

    private boolean isRecordType(String type) {
        return type.contains("[key:") || type.startsWith("Record<");
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        parameter.dataType = applyLocalTypeMapping(parameter.dataType);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap operations, List<ModelMap> allModels) {
        OperationMap objectMap = operations.getOperations();
        List<CodegenOperation> operationList = objectMap.getOperation();

        for (CodegenOperation operation : operationList) {
            operation.path = operation.path.replaceAll("\\{([^}]+)}", ":$1");
            operation.httpMethod = camelize(operation.httpMethod.toLowerCase(Locale.ROOT));

            List<CodegenParameter> params = operation.allParams;
            if (params != null && params.isEmpty()) {
                operation.allParams = null;
            }
            List<CodegenResponse> responses = operation.responses;
            if (responses != null) {
                for (CodegenResponse resp : responses) {
                    if ("0".equals(resp.code)) {
                        resp.code = "default";
                    }
                }
            }
            if (operation.examples != null && !operation.examples.isEmpty()) {
                // Leave application/json* items only
                for (Iterator<Map<String, String>> it = operation.examples.iterator(); it.hasNext(); ) {
                    final Map<String, String> example = it.next();
                    final String contentType = example.get("contentType");
                    if (contentType == null || !contentType.startsWith("application/json")) {
                        it.remove();
                    }
                }
            }
        }

        // Collect and deduplicate imports for the entire API (all operations)
        Set<String> allImports = new TreeSet<>();
        Set<String> httpMethods = new TreeSet<>();

        for (CodegenOperation operation : operationList) {
            // Collect HTTP methods for controller imports
            String httpMethod = operation.httpMethod;
            if (httpMethod != null) {
                httpMethods.add(httpMethod);
            }

            // Collect imports from parameters
            if (operation.allParams != null) {
                for (CodegenParameter param : operation.allParams) {
                    if(param.dataType != null) {
                        if(isLanguageGenericType(param.dataType)) {
                            // Extract generic type and add to imports if its not a primitive
                            String genericType = extractGenericType(param.dataType);
                            if (genericType != null && !isLanguagePrimitive(genericType) && !isRecordType(genericType)) {
                                allImports.add(genericType);
                            }
                        } else if (!isLanguagePrimitive(param.dataType) && !isRecordType(param.dataType)) {
                            allImports.add(param.dataType);
                        }
                    }

                }
            }

            // Collect imports from return type
            if (operation.returnType != null
                    && !isLanguagePrimitive(operation.returnType)
                    && !isRecordType(operation.returnType)
            ) {
                if (isLanguageGenericType(operation.returnType)) {
                    // Extract generic type and add to imports if it's not a primitive
                    String genericType = extractGenericType(operation.returnType);
                    if (genericType != null && !isLanguagePrimitive(genericType) && !isRecordType(genericType)) {
                        allImports.add(genericType);
                    }
                } else {
                    allImports.add(operation.returnType);
                }
            }
        }

        // Convert imports to the format expected by templates
        List<Map<String, String>> tsImports = new ArrayList<>();
        for (String importName : allImports) {
            HashMap<String, String> tsImport = new HashMap<>();
            tsImport.put("classname", importName);
            tsImports.add(tsImport);
        }

        // Store the deduplicated imports and HTTP methods in the operations map for use by templates
        operations.put("tsImports", tsImports);
        operations.put("httpMethods", httpMethods);

        return operations;
    }

    private String extractGenericType(String type) {
        int startAngleBracketIndex = type.indexOf('<');
        int endAngleBracketIndex = type.lastIndexOf('>');
        if (startAngleBracketIndex < 0 || endAngleBracketIndex < 0) {
            return null;
        }
        String genericType = type.substring(startAngleBracketIndex + 1, endAngleBracketIndex);
        if(isLanguageGenericType(genericType)) {
            return extractGenericType(type);
        }
        if(genericType.contains("|")) {
            return null;
        }
        return genericType;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> result = super.postProcessAllModels(objs);
        for (ModelsMap entry : result.values()) {
            for (ModelMap mo : entry.getModels()) {
                CodegenModel cm = mo.getModel();
                // Add additional filename information for imports
                Set<String> parsedImports = parseImports(cm);
                mo.put("tsImports", toTsImports(cm, parsedImports));
            }
        }
        return result;
    }

    /**
     * Parse imports
     */
    private Set<String> parseImports(CodegenModel cm) {
        Set<String> newImports = new HashSet<>();
        if (cm.imports.size() > 0) {
            for (String name : cm.imports) {
                if (name.indexOf(" | ") >= 0) {
                    String[] parts = name.split(" \\| ");
                    Collections.addAll(newImports, parts);
                } else {
                    newImports.add(name);
                }
            }
        }
        return newImports;
    }

    private List<Map<String, String>> toTsImports(CodegenModel cm, Set<String> imports) {
        List<Map<String, String>> tsImports = new ArrayList<>();
        for (String im : imports) {
            if (!im.equals(cm.classname)) {
                HashMap<String, String> tsImport = new HashMap<>();
                // TVG: This is used as class name in the import statements of the model file
                tsImport.put("classname", im);
                tsImport.put("filename", toModelFilename(removeModelPrefixSuffix(im)));
                tsImports.add(tsImport);
            }
        }
        return tsImports;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        ModelsMap result = super.postProcessModels(objs);
        return postProcessModelsEnum(result);
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "default";
        }
        return camelize(name) + apiSuffix;
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if (templateName.endsWith("controller.mustache")) {
            StringBuilder sb = new StringBuilder(result);
            int fileTypeSeparator = result.lastIndexOf(".");
            sb.insert(fileTypeSeparator, ".controller");
            String packageName = apiPackage();
            int packageNameIndex = result.lastIndexOf(packageName);
            sb.replace(packageNameIndex, packageNameIndex + packageName.length(), "controllers");
            return sb.toString();
        }

        return result;
    }

    @Override
    public String toApiVarName(String name) {
        return super.toApiVarName(name) + apiSuffix;
    }

    @Override
    public String toApiImport(String name) {
        return toApiFilename(name) + apiNameSuffix;
    }

    @Override
    public String toModelFilename(String name) {
        return convertUsingFileNamingConvention(name) + modelFileSuffix;
    }

    @Override
    public String toModelImport(String name) {
        return toModelFilename(name) + modelFileSuffix;
    }

    @Override
    public String toModelName(String name) {
        String modelName = super.toModelName(name);
        if (modelSuffix.length() == 0 || modelName.endsWith(modelSuffix)) {
            return modelName;
        }
        return modelName + modelSuffix;
    }

    public String removeModelPrefixSuffix(String name) {
        String result = name;
        if (modelSuffix.length() > 0 && result.endsWith(modelSuffix)) {
            result = result.substring(0, result.length() - modelSuffix.length());
        }
        String prefix = capitalize(this.modelNamePrefix);
        String suffix = capitalize(this.modelNameSuffix);
        if (prefix.length() > 0 && result.startsWith(prefix)) {
            result = result.substring(prefix.length());
        }
        if (suffix.length() > 0 && result.endsWith(suffix)) {
            result = result.substring(0, result.length() - suffix.length());
        }

        return result;
    }

    /**
     * Validates that the given string value only contains '-', '.' and alpha numeric characters.
     * Throws an IllegalArgumentException, if the string contains any other characters.
     *
     * @param argument The name of the argument being validated. This is only used for displaying an error message.
     * @param value    The value that is being validated.
     */
    private void validateFileSuffixArgument(String argument, String value) {
        if (value != null && !value.matches(FILE_NAME_SUFFIX_PATTERN)) {
            throw new IllegalArgumentException(
                    String.format(Locale.ROOT, "%s file suffix only allows '.', '-' and alphanumeric characters.", argument)
            );
        }
    }

    /**
     * Validates that the given string value only contains alpha numeric characters.
     * Throws an IllegalArgumentException, if the string contains any other characters.
     *
     * @param argument The name of the argument being validated. This is only used for displaying an error message.
     * @param value    The value that is being validated.
     */
    private void validateClassSuffixArgument(String argument, String value) {
        if (value != null && !value.matches(CLASS_NAME_SUFFIX_PATTERN)) {
            throw new IllegalArgumentException(
                    String.format(Locale.ROOT, "%s class suffix only allows alphanumeric characters.", argument)
            );
        }
    }

    /**
     * Set the file naming type.
     *
     * @param fileNaming the file naming to use
     */
    private void setFileNaming(String fileNaming) {
        if ("camelCase".equals(fileNaming) || "kebab-case".equals(fileNaming)) {
            this.fileNaming = fileNaming;
        } else {
            throw new IllegalArgumentException("Invalid file naming '" +
                    fileNaming + "'. Must be 'camelCase' or 'kebab-case'");
        }
    }

    /**
     * Converts the original name according to the current <code>fileNaming</code> strategy.
     *
     * @param originalName the original name to transform
     * @return the transformed name
     */
    private String convertUsingFileNamingConvention(String originalName) {
        String name = this.removeModelPrefixSuffix(originalName);
        if ("kebab-case".equals(fileNaming)) {
            name = dashize(underscore(name));
        } else {
            name = camelize(name, LOWERCASE_FIRST_LETTER);
        }
        return name;
    }
}
