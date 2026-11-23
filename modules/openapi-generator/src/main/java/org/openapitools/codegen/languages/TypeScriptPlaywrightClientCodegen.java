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
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import lombok.Getter;
import lombok.Setter;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.IndentedLambda;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.*;

/**
 * Generates a self-contained <a href="https://playwright.dev/">Playwright</a> test
 * project (a typed API client plus generated API/model specs) from an OpenAPI document.
 * The generated client wraps Playwright's {@code APIRequestContext} (the {@code request}
 * fixture), so the tests issue, trace and report requests through Playwright.
 */
public class TypeScriptPlaywrightClientCodegen extends AbstractTypeScriptClientCodegen {
    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String WITH_INTERFACES = "withInterfaces";
    public static final String USE_SINGLE_REQUEST_PARAMETER = "useSingleRequestParameter";
    public static final String PREFIX_PARAMETER_INTERFACES = "prefixParameterInterfaces";
    public static final String WITHOUT_RUNTIME_CHECKS = "withoutRuntimeChecks";
    public static final String STRING_ENUMS = "stringEnums";
    public static final String STRING_ENUMS_DESC = "Generate string enums instead of objects for enum values.";
    public static final String IMPORT_FILE_EXTENSION_SWITCH = "importFileExtension";
    public static final String IMPORT_FILE_EXTENSION_SWITCH_DESC = "File extension to use with relative imports. Set it to '.js' or '.mjs' when using [ESM](https://nodejs.org/api/esm.html).";
    public static final String FILE_NAMING = "fileNaming";
    public static final String KEBAB_CASE = "kebab-case";
    public static final String CAMEL_CASE = "camelCase";
    public static final String PASCAL_CASE = "PascalCase";
    public static final String USE_SQUARE_BRACKETS_IN_ARRAY_NAMES = "useSquareBracketsInArrayNames";

    @Getter
    @Setter
    protected String npmRepository = null;
    @Getter
    @Setter
    protected String importFileExtension = "";
    private boolean useSingleRequestParameter = true;
    private boolean prefixParameterInterfaces = false;
    protected boolean addedApiIndex = false;
    protected boolean addedModelIndex = false;
    protected boolean withoutRuntimeChecks = false;
    protected boolean stringEnums = false;
    protected String fileNaming = PASCAL_CASE;
    protected String testFolder = "spec";

    public TypeScriptPlaywrightClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .includeSecurityFeatures(SecurityFeature.BearerToken));

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "generated-code/typescript-playwright";
        embeddedTemplateDir = templateDir = "typescript-playwright";

        this.apiTemplateFiles.put("apis.mustache", ".ts");
        this.modelTestTemplateFiles.put("model_test.mustache", ".ts");
        this.apiTestTemplateFiles.put("api_test.mustache", ".ts");

        this.addExtraReservedWords();

        supportModelPropertyNaming(CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.camelCase);
        this.cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url your private npmRepo in the package.json"));
        this.cliOptions.add(new CliOption(WITH_INTERFACES, "Setting this property to true will generate interfaces next to the default class implementations.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, CodegenConstants.USE_SINGLE_REQUEST_PARAMETER_DESC, SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.TRUE.toString()));
        this.cliOptions.add(new CliOption(PREFIX_PARAMETER_INTERFACES, "Setting this property to true will generate parameter interface declarations prefixed with API class name to avoid name conflicts.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(WITHOUT_RUNTIME_CHECKS, "Setting this property to true will remove any runtime checks on the request and response payloads. Payloads will be casted to their expected types.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(STRING_ENUMS, STRING_ENUMS_DESC, SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(IMPORT_FILE_EXTENSION_SWITCH, IMPORT_FILE_EXTENSION_SWITCH_DESC).defaultValue(""));
        this.cliOptions.add(new CliOption(FILE_NAMING, "Naming convention for the output files: 'PascalCase', 'camelCase', 'kebab-case'.").defaultValue(this.fileNaming));
        this.cliOptions.add(new CliOption(USE_SQUARE_BRACKETS_IN_ARRAY_NAMES, "Setting this property to true will add brackets to array attribute names, e.g. my_values[].", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    }

    @Override
    public String toApiFilename(String name) {
        return convertUsingFileNamingConvention(super.toApiFilename(name));
    }

    @Override
    public String toModelFilename(String name) {
        return convertUsingFileNamingConvention(super.toModelFilename(name));
    }

    /**
     * Converts the original name according to the current <code>fileNaming</code> strategy.
     *
     * @param originalName the original name to transform
     * @return the transformed name
     */
    private String convertUsingFileNamingConvention(String originalName) {
        String name = originalName;
        if (KEBAB_CASE.equals(fileNaming)) {
            name = dashize(underscore(name));
        } else if (CAMEL_CASE.equals(fileNaming)) {
            name = camelize(name, LOWERCASE_FIRST_LETTER);
        }
        return name;
    }

    @Override
    public String getName() {
        return "typescript-playwright";
    }

    @Override
    public String getHelp() {
        return "Generates end-to-end tests using Playwright for TypeScript.";
    }

    public Boolean getWithoutRuntimeChecks() {
        return withoutRuntimeChecks;
    }

    public void setWithoutRuntimeChecks(Boolean withoutRuntimeChecks) {
        this.withoutRuntimeChecks = withoutRuntimeChecks;
    }

    public Boolean getStringEnums() {
        return this.stringEnums;
    }

    public void setStringEnums(Boolean stringEnums) {
        this.stringEnums = stringEnums;
    }

    /**
     * Set the file naming type.
     *
     * @param fileNaming the file naming to use
     */
    public void setFileNaming(String fileNaming) {
        if (PASCAL_CASE.equals(fileNaming) || CAMEL_CASE.equals(fileNaming) || KEBAB_CASE.equals(fileNaming)) {
            this.fileNaming = fileNaming;
        } else {
            throw new IllegalArgumentException("Invalid file naming '" +
                    fileNaming + "'. Must be 'PascalCase', 'camelCase' or 'kebab-case'");
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();
        additionalProperties.put("isOriginalModelPropertyNaming", getModelPropertyNaming() == CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.original);
        additionalProperties.put("modelPropertyNaming", getModelPropertyNaming().name());

        // The generated artifact is a self-contained Playwright test project, so the
        // client, models and tests all live at the project root (no "src" nesting).
        String sourceDir = "";

        this.apiPackage = sourceDir + "apis";
        this.modelPackage = sourceDir + "models";

        supportingFiles.add(new SupportingFile("index.mustache", sourceDir, "index.ts"));
        supportingFiles.add(new SupportingFile("runtime.mustache", sourceDir, "runtime.ts"));

        if (additionalProperties.containsKey(IMPORT_FILE_EXTENSION_SWITCH)) {
            this.setImportFileExtension(additionalProperties.get(IMPORT_FILE_EXTENSION_SWITCH).toString());
        }
        writePropertyBack(IMPORT_FILE_EXTENSION_SWITCH, getImportFileExtension());

        if (additionalProperties.containsKey(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER)) {
            this.setUseSingleRequestParameter(convertPropertyToBoolean(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER));
        }
        writePropertyBack(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, getUseSingleRequestParameter());

        if (additionalProperties.containsKey(PREFIX_PARAMETER_INTERFACES)) {
            this.setPrefixParameterInterfaces(convertPropertyToBoolean(PREFIX_PARAMETER_INTERFACES));
        }
        writePropertyBack(PREFIX_PARAMETER_INTERFACES, getPrefixParameterInterfaces());

        if (additionalProperties.containsKey(WITHOUT_RUNTIME_CHECKS)) {
            this.setWithoutRuntimeChecks(convertPropertyToBoolean(WITHOUT_RUNTIME_CHECKS));
        }

        if (additionalProperties.containsKey(STRING_ENUMS)) {
            this.setStringEnums(convertPropertyToBoolean(STRING_ENUMS));
        }

        if (additionalProperties.containsKey(FILE_NAMING)) {
            this.setFileNaming(additionalProperties.get(FILE_NAMING).toString());
        }

        if (!withoutRuntimeChecks) {
            this.modelTemplateFiles.put("models.mustache", ".ts");
            typeMapping.put("date", "Date");
            typeMapping.put("DateTime", "Date");
        }

        // Always generate the supporting files that make the output a runnable,
        // type-checkable Playwright project.
        addProjectFileGeneration();
    }

    @Override
    protected ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas() {
        ImmutableMap.Builder<String, Mustache.Lambda> lambdas = super.addMustacheLambdas();
        lambdas.put("indented_star_1", new IndentedLambda(1, " ", "* ", false, false));
        lambdas.put("indented_star_4", new IndentedLambda(5, " ", "* ", false, false));
        return lambdas;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isFileSchema(p)) {
            return "Blob";
        } else if (ModelUtils.isBinarySchema(p)) {
            return "Blob";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        codegenModel.additionalPropertiesType = getTypeDeclaration(ModelUtils.getAdditionalProperties(schema));
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        List<ModelMap> models = postProcessModelsEnum(objs).getModels();

        // process enum and custom properties in models
        for (ModelMap mo : models) {
            ExtendedCodegenModel cm = (ExtendedCodegenModel) mo.getModel();
            cm.imports = new TreeSet<>(cm.imports);
            this.processCodeGenModel(cm);
        }

        // Add supporting file only if we plan to generate files in /models
        if (!objs.isEmpty() && !addedModelIndex) {
            addedModelIndex = true;
            supportingFiles.add(new SupportingFile("models.index.mustache", modelPackage().replace('.', File.separatorChar), "index.ts"));
        }

        return objs;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        if (parameter.isFormParam && parameter.isArray && "binary".equals(parameter.dataFormat)) {
            parameter.isCollectionFormatMulti = true;
        }
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        List<ExtendedCodegenModel> allModels = new ArrayList<>();

        Map<String, ModelsMap> result = super.postProcessAllModels(objs);
        for (ModelsMap entry : result.values()) {
            for (ModelMap model : entry.getModels()) {
                ExtendedCodegenModel codegenModel = (ExtendedCodegenModel) model.getModel();
                model.put("hasImports", codegenModel.imports.size() > 0);
                model.put("tsImports", toTsImports(codegenModel, parseImports(codegenModel)));
                allModels.add(codegenModel);
            }
        }

        for (ExtendedCodegenModel rootModel : allModels) {
            for (String curImport : rootModel.imports) {
                boolean isModelImport = false;
                for (ExtendedCodegenModel model : allModels) {
                    if (model.classname.equals(curImport) && !model.isEnum) {
                        isModelImport = true;
                        break;
                    }
                }
                if (isModelImport) {
                    rootModel.modelImports.add(curImport);
                }
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
                tsImport.put("filename", toModelFilename(im));
                tsImports.add(tsImport);
            }
        }
        return tsImports;
    }

    private void addProjectFileGeneration() {

        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }

        // Files that make the generated output a self-contained Playwright test project.
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        supportingFiles.add(new SupportingFile("playwright.config.mustache", "", "playwright.config.ts"));
        supportingFiles.add(new SupportingFile("npmignore.mustache", "", ".npmignore"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
    }

    @Override
    public List<CodegenParameter> fromRequestBodyToFormParameters(RequestBody body, Set<String> imports) {
        List<CodegenParameter> superParams = super.fromRequestBodyToFormParameters(body, imports);
        List<CodegenParameter> extendedParams = new ArrayList<CodegenParameter>();
        for (CodegenParameter cp : superParams) {
            extendedParams.add(new ExtendedCodegenParameter(cp));
        }
        return extendedParams;
    }

    @Override
    public ExtendedCodegenParameter fromParameter(Parameter parameter, Set<String> imports) {
        CodegenParameter cp = super.fromParameter(parameter, imports);
        return new ExtendedCodegenParameter(cp);
    }

    @Override
    public CodegenParameter fromFormProperty(String name, Schema propertySchema, Set<String> imports) {
        CodegenParameter cp = super.fromFormProperty(name, propertySchema, imports);
        return new ExtendedCodegenParameter(cp);
    }

    @Override
    public CodegenParameter fromRequestBody(RequestBody body, Set<String> imports, String bodyParameterName) {
        CodegenParameter cp = super.fromRequestBody(body, imports, bodyParameterName);
        return new ExtendedCodegenParameter(cp);
    }

    @Override
    public ExtendedCodegenProperty fromProperty(String name, Schema p, boolean required) {
        CodegenProperty cp = super.fromProperty(name, p, required);
        return new ExtendedCodegenProperty(cp);
    }

    @Override
    public ExtendedCodegenModel fromModel(String name, Schema model) {
        CodegenModel cm = super.fromModel(name, model);
        return new ExtendedCodegenModel(cm);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap operations, List<ModelMap> allModels) {
        // Add supporting file only if we plan to generate files in /apis
        if (!operations.isEmpty() && !addedApiIndex) {
            addedApiIndex = true;
            supportingFiles.add(new SupportingFile("apis.index.mustache", apiPackage().replace('.', File.separatorChar), "index.ts"));
        }

        // Add supporting file only if we plan to generate files in /models
        if (!allModels.isEmpty() && !addedModelIndex) {
            addedModelIndex = true;
            supportingFiles.add(new SupportingFile("models.index.mustache", modelPackage().replace('.', File.separatorChar), "index.ts"));
        }

        this.addOperationModelImportInformation(operations);
        this.updateOperationParameterForEnum(operations);
        this.addOperationObjectResponseInformation(operations);
        this.addOperationPrefixParameterInterfacesInformation(operations);
        this.escapeOperationIds(operations);
        this.addPlaywrightTestData(operations);
        return operations;
    }

    private ExtendedCodegenModel processCodeGenModel(ExtendedCodegenModel cm) {
        for (CodegenProperty cpVar : cm.vars) {
            ExtendedCodegenProperty var = (ExtendedCodegenProperty) cpVar;
            this.processCodegenProperty(var, cm.classname);
        }

        if (cm.parent != null) {
            for (CodegenProperty cpVar : cm.allVars) {
                ExtendedCodegenProperty var = (ExtendedCodegenProperty) cpVar;

                if (Boolean.TRUE.equals(var.isEnum)) {
                    var.datatypeWithEnum = var.datatypeWithEnum
                            .replace(var.enumName, cm.classname + var.enumName);
                }
            }
        }
        if (!cm.oneOf.isEmpty()) {
            // For oneOfs only import $refs within the oneOf
            TreeSet<String> oneOfRefs = new TreeSet<>();
            for (String im : cm.imports) {
                if (cm.oneOf.contains(im)) {
                    oneOfRefs.add(im);
                }
            }
            cm.imports = oneOfRefs;
        }
        return cm;
    }

    private void processCodegenProperty(ExtendedCodegenProperty var, String parentClassName) {
        // name enum with model name, e.g. StatusEnum => PetStatusEnum
        if (Boolean.TRUE.equals(var.isEnum)) {
            // behaviour for enum names is specific for Typescript Fetch, not using namespaces
            var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, parentClassName + var.enumName);

            // need to post-process defaultValue, was computed with previous var.datatypeWithEnum
            if (var.defaultValue != null && !var.defaultValue.equals("undefined")) {
                int dotPos = var.defaultValue.indexOf(".");
                if (dotPos != -1) {
                    var.defaultValue = var.datatypeWithEnum + var.defaultValue.substring(dotPos);
                }
            }
        }
    }

    private void escapeOperationIds(OperationsMap operations) {
        for (CodegenOperation op : operations.getOperations().getOperation()) {
            String param = op.operationIdCamelCase + "Request";
            if (op.imports.contains(param)) {
                // we import a model with the same name as the generated operation, escape it
                op.operationIdCamelCase += "Operation";
                op.operationIdLowerCase += "operation";
                op.operationIdSnakeCase += "_operation";
            }
        }
    }

    private void addOperationModelImportInformation(OperationsMap operations) {
        // This method will add extra information to the operations.imports array.
        // The api template uses this information to import all the required
        // models for a given operation.
        List<Map<String, String>> imports = operations.getImports();
        for (Map<String, String> im : imports) {
            String className = im.get("import").replace(modelPackage() + ".", "");
            im.put("className", className);
        }
    }

    private void updateOperationParameterForEnum(OperationsMap operations) {
        // This method will add extra information as to whether or not we have enums and
        // update their names with the operation.id prefixed.
        boolean hasEnum = false;
        for (CodegenOperation op : operations.getOperations().getOperation()) {
            for (CodegenParameter param : op.allParams) {
                if (Boolean.TRUE.equals(param.isEnum)) {
                    hasEnum = true;
                    param.datatypeWithEnum = param.datatypeWithEnum
                            .replace(param.enumName, op.operationIdCamelCase + param.enumName);
                }
            }
        }

        operations.put("hasEnums", hasEnum);
    }

    private void addOperationObjectResponseInformation(OperationsMap operations) {
        // This method will modify the information on the operations' return type.
        // The api template uses this information to know when to return a text
        // response for a given simple response operation.
        for (CodegenOperation op : operations.getOperations().getOperation()) {
            if ("object".equals(op.returnType)) {
                op.isMap = true;
                op.returnSimpleType = false;
            }
        }
    }

    private void addOperationPrefixParameterInterfacesInformation(Map<String, Object> operations) {
        operations.put("prefixParameterInterfaces", getPrefixParameterInterfaces());
    }

    /**
     * Enrich each operation with the data the Playwright test templates need:
     * a placeholder example value for every required parameter
     * ({@code x-playwright-example}) and the expected success status code
     * ({@code x-playwright-status}).
     */
    private void addPlaywrightTestData(OperationsMap operations) {
        for (CodegenOperation op : operations.getOperations().getOperation()) {
            op.vendorExtensions.put("x-playwright-status", getSuccessStatusCode(op));
            op.vendorExtensions.put("x-playwright-test-name", getTestName(op));
            for (CodegenParameter param : op.allParams) {
                if (param.required) {
                    param.vendorExtensions.put("x-playwright-example", getParameterExampleValue(param));
                }
            }
        }
    }

    /**
     * Builds the (single-quote safe) title for a generated Playwright test: the
     * operation nickname, optionally suffixed with its summary. The result is escaped
     * so summaries containing apostrophes, backslashes or newlines still produce valid
     * TypeScript.
     */
    private String getTestName(CodegenOperation op) {
        String name = op.nickname;
        if (op.summary != null && !op.summary.isEmpty()) {
            name += " - " + op.summary;
        }
        return escapeForSingleQuotedString(name);
    }

    /**
     * Escapes a string so it can be safely embedded inside a single-quoted TypeScript
     * string literal.
     */
    private String escapeForSingleQuotedString(String value) {
        if (value == null) {
            return "";
        }
        return value
                .replace("\\", "\\\\")
                .replace("'", "\\'")
                .replace("\r", "\\r")
                .replace("\n", "\\n")
                .replace("\t", "\\t");
    }

    /**
     * Returns the first 2xx response code declared for the operation, falling back to
     * {@code 200} when none can be determined.
     */
    private String getSuccessStatusCode(CodegenOperation op) {
        if (op.responses != null) {
            for (CodegenResponse response : op.responses) {
                if (response.is2xx && response.code != null && response.code.matches("\\d{3}")) {
                    return response.code;
                }
            }
        }
        return "200";
    }

    /**
     * Returns a TypeScript expression usable as a placeholder value for the given
     * parameter inside a generated Playwright test. The value is type-compatible with
     * the parameter so the generated test type-checks; it is not meant to be
     * semantically meaningful.
     */
    private String getParameterExampleValue(CodegenParameter param) {
        if (param.isArray) {
            return "[]";
        }
        if (Boolean.TRUE.equals(param.isEnum) && param._enum != null && !param._enum.isEmpty()) {
            String first = String.valueOf(param._enum.get(0));
            return param.isString ? "'" + escapeForSingleQuotedString(first) + "'" : first;
        }
        if (param.isBoolean) {
            return "true";
        }
        if (param.isInteger || param.isLong || param.isNumber || param.isFloat
                || param.isDouble || param.isDecimal || param.isNumeric) {
            return "1";
        }
        if (param.isDate || param.isDateTime) {
            return "new Date()";
        }
        if (param.isFile || param.isBinary) {
            return "new Blob([])";
        }
        if (param.isString || param.isUuid || param.isUri || param.isEmail) {
            return "'" + param.paramName + "_example'";
        }
        // models, free-form objects, maps and anything else: cast an empty object to the
        // declared type so the generated test still type-checks.
        return "{} as unknown as " + param.dataType;
    }

    private void addExtraReservedWords() {
        this.reservedWords.add("BASE_PATH");
        this.reservedWords.add("BaseAPI");
        this.reservedWords.add("RequiredError");
        this.reservedWords.add("COLLECTION_FORMATS");
        this.reservedWords.add("FetchAPI");
        this.reservedWords.add("ConfigurationParameters");
        this.reservedWords.add("Configuration");
        this.reservedWords.add("configuration");
        this.reservedWords.add("HTTPMethod");
        this.reservedWords.add("HTTPHeaders");
        this.reservedWords.add("HTTPQuery");
        this.reservedWords.add("HTTPBody");
        this.reservedWords.add("ModelPropertyNaming");
        this.reservedWords.add("FetchParams");
        this.reservedWords.add("RequestOpts");
        this.reservedWords.add("exists");
        this.reservedWords.add("RequestContext");
        this.reservedWords.add("ResponseContext");
        this.reservedWords.add("Middleware");
        this.reservedWords.add("ApiResponse");
        this.reservedWords.add("ResponseTransformer");
        this.reservedWords.add("JSONApiResponse");
        this.reservedWords.add("VoidApiResponse");
        this.reservedWords.add("BlobApiResponse");
        this.reservedWords.add("TextApiResponse");
        // "Index" would create a file "Index.ts" which on case insensitive filesystems
        // would override our "index.js" file
        this.reservedWords.add("Index");
    }

    private boolean getUseSingleRequestParameter() {
        return useSingleRequestParameter;
    }

    private void setUseSingleRequestParameter(boolean useSingleRequestParameter) {
        this.useSingleRequestParameter = useSingleRequestParameter;
    }

    private boolean getPrefixParameterInterfaces() {
        return prefixParameterInterfaces;
    }

    private void setPrefixParameterInterfaces(boolean prefixParameterInterfaces) {
        this.prefixParameterInterfaces = prefixParameterInterfaces;
    }

    class ExtendedCodegenParameter extends CodegenParameter {
        public List<CodegenProperty> readOnlyVars; // a list of read-only properties
        public boolean hasReadOnly = false; // indicates the type has at least one read-only property

        public boolean isDateType() {
            return isDate && "Date".equals(dataType);
        }

        public boolean isDateTimeType() {
            return isDateTime && "Date".equals(dataType);
        }

        public ExtendedCodegenParameter(CodegenParameter cp) {
            super();

            this.isFormParam = cp.isFormParam;
            this.isQueryParam = cp.isQueryParam;
            this.isPathParam = cp.isPathParam;
            this.isHeaderParam = cp.isHeaderParam;
            this.isCookieParam = cp.isCookieParam;
            this.isBodyParam = cp.isBodyParam;
            this.isContainer = cp.isContainer;
            this.isCollectionFormatMulti = cp.isCollectionFormatMulti;
            this.isPrimitiveType = cp.isPrimitiveType;
            this.isModel = cp.isModel;
            this.isExplode = cp.isExplode;
            this.baseName = cp.baseName;
            this.paramName = cp.paramName;
            this.dataType = cp.dataType;
            this.datatypeWithEnum = cp.datatypeWithEnum;
            this.dataFormat = cp.dataFormat;
            this.contentType = cp.contentType;
            this.collectionFormat = cp.collectionFormat;
            this.description = cp.description;
            this.unescapedDescription = cp.unescapedDescription;
            this.baseType = cp.baseType;
            this.defaultValue = cp.defaultValue;
            this.enumName = cp.enumName;
            this.style = cp.style;
            this.nameInLowerCase = cp.nameInLowerCase;
            this.example = cp.example;
            this.jsonSchema = cp.jsonSchema;
            this.isString = cp.isString;
            this.isNumeric = cp.isNumeric;
            this.isInteger = cp.isInteger;
            this.isLong = cp.isLong;
            this.isNumber = cp.isNumber;
            this.isFloat = cp.isFloat;
            this.isDouble = cp.isDouble;
            this.isDecimal = cp.isDecimal;
            this.isByteArray = cp.isByteArray;
            this.isBinary = cp.isBinary;
            this.isBoolean = cp.isBoolean;
            this.isDate = cp.isDate;
            this.isDateTime = cp.isDateTime;
            this.isUuid = cp.isUuid;
            this.isUri = cp.isUri;
            this.isEmail = cp.isEmail;
            this.isFreeFormObject = cp.isFreeFormObject;
            this.isAnyType = cp.isAnyType;
            this.isArray = cp.isArray;
            this.isMap = cp.isMap;
            this.isFile = cp.isFile;
            this.isEnum = cp.isEnum;
            this._enum = cp._enum;
            this.allowableValues = cp.allowableValues;
            this.items = cp.items;
            this.additionalProperties = cp.additionalProperties;
            this.vars = cp.vars;
            this.requiredVars = cp.requiredVars;
            this.mostInnerItems = cp.mostInnerItems;
            this.vendorExtensions = cp.vendorExtensions;
            this.hasValidation = cp.hasValidation;
            this.isNullable = cp.isNullable;
            this.required = cp.required;
            this.maximum = cp.maximum;
            this.exclusiveMaximum = cp.exclusiveMaximum;
            this.minimum = cp.minimum;
            this.exclusiveMinimum = cp.exclusiveMinimum;
            this.maxLength = cp.maxLength;
            this.minLength = cp.minLength;
            this.pattern = cp.pattern;
            this.maxItems = cp.maxItems;
            this.minItems = cp.minItems;
            this.uniqueItems = cp.uniqueItems;
            this.multipleOf = cp.multipleOf;
            this.setHasVars(cp.getHasVars());
            this.setHasRequired(cp.getHasRequired());
            this.setMaxProperties(cp.getMaxProperties());
            this.setMinProperties(cp.getMinProperties());
            setReadOnlyVars();
        }

        @Override
        public ExtendedCodegenParameter copy() {
            return new ExtendedCodegenParameter(super.copy());
        }

        private void setReadOnlyVars() {
            readOnlyVars = vars.stream().filter(v -> v.isReadOnly).collect(Collectors.toList());
            hasReadOnly = !readOnlyVars.isEmpty();
        }
    }

    class ExtendedCodegenProperty extends CodegenProperty {
        public boolean isDateType() {
            return isDate && "Date".equals(dataType);
        }

        public boolean isDateTimeType() {
            return isDateTime && "Date".equals(dataType);
        }

        public ExtendedCodegenProperty(CodegenProperty cp) {
            super();

            this.openApiType = cp.openApiType;
            this.baseName = cp.baseName;
            this.complexType = cp.complexType;
            this.getter = cp.getter;
            this.setter = cp.setter;
            this.description = cp.description;
            this.dataType = cp.dataType;
            this.datatypeWithEnum = cp.datatypeWithEnum;
            this.dataFormat = cp.dataFormat;
            this.name = cp.name;
            this.min = cp.min;
            this.max = cp.max;
            this.defaultValue = cp.defaultValue;
            this.defaultValueWithParam = cp.defaultValueWithParam;
            this.baseType = cp.baseType;
            this.containerType = cp.containerType;
            this.title = cp.title;
            this.unescapedDescription = cp.unescapedDescription;
            this.maxLength = cp.maxLength;
            this.minLength = cp.minLength;
            this.pattern = cp.pattern;
            this.example = cp.example;
            this.jsonSchema = cp.jsonSchema;
            this.minimum = cp.minimum;
            this.maximum = cp.maximum;
            this.multipleOf = cp.multipleOf;
            this.exclusiveMinimum = cp.exclusiveMinimum;
            this.exclusiveMaximum = cp.exclusiveMaximum;
            this.required = cp.required;
            this.deprecated = cp.deprecated;
            this.isPrimitiveType = cp.isPrimitiveType;
            this.isModel = cp.isModel;
            this.isContainer = cp.isContainer;
            this.isString = cp.isString;
            this.isNumeric = cp.isNumeric;
            this.isInteger = cp.isInteger;
            this.isLong = cp.isLong;
            this.isNumber = cp.isNumber;
            this.isFloat = cp.isFloat;
            this.isDouble = cp.isDouble;
            this.isDecimal = cp.isDecimal;
            this.isByteArray = cp.isByteArray;
            this.isBinary = cp.isBinary;
            this.isFile = cp.isFile;
            this.isBoolean = cp.isBoolean;
            this.isDate = cp.isDate; // full-date notation as defined by RFC 3339, section 5.6, for example, 2017-07-21
            this.isDateTime = cp.isDateTime; // the date-time notation as defined by RFC 3339, section 5.6, for example, 2017-07-21T17:32:28Z
            this.isUuid = cp.isUuid;
            this.isUri = cp.isUri;
            this.isEmail = cp.isEmail;
            this.isFreeFormObject = cp.isFreeFormObject;
            this.isAnyType = cp.isAnyType;
            this.isArray = cp.isArray;
            this.isMap = cp.isMap;
            this.isEnum = cp.isEnum;
            this.isReadOnly = cp.isReadOnly;
            this.isWriteOnly = cp.isWriteOnly;
            this.isNullable = cp.isNullable;
            this.isSelfReference = cp.isSelfReference;
            this.isCircularReference = cp.isCircularReference;
            this.isDiscriminator = cp.isDiscriminator;
            this._enum = cp._enum;
            this.allowableValues = cp.allowableValues;
            this.items = cp.items;
            this.additionalProperties = cp.additionalProperties;
            this.vars = cp.vars;
            this.requiredVars = cp.requiredVars;
            this.mostInnerItems = cp.mostInnerItems;
            this.vendorExtensions = cp.vendorExtensions;
            this.hasValidation = cp.hasValidation;
            this.isInherited = cp.isInherited;
            this.discriminatorValue = cp.discriminatorValue;
            this.nameInLowerCase = cp.nameInLowerCase;
            this.nameInPascalCase = cp.nameInPascalCase;
            this.nameInSnakeCase = cp.nameInSnakeCase;
            this.enumName = cp.enumName;
            this.maxItems = cp.maxItems;
            this.minItems = cp.minItems;
            this.setMaxProperties(cp.getMaxProperties());
            this.setMinProperties(cp.getMinProperties());
            this.setUniqueItems(cp.getUniqueItems());
            this.isXmlAttribute = cp.isXmlAttribute;
            this.xmlPrefix = cp.xmlPrefix;
            this.xmlName = cp.xmlName;
            this.xmlNamespace = cp.xmlNamespace;
            this.isXmlWrapped = cp.isXmlWrapped;
        }
    }

    class ExtendedCodegenModel extends CodegenModel {
        @Getter
        @Setter
        public Set<String> modelImports = new TreeSet<String>();

        public boolean isDateType() {
            return isDate && "Date".equals(dataType);
        }

        public boolean isDateTimeType() {
            return isDateTime && "Date".equals(dataType);
        }

        public ExtendedCodegenModel(CodegenModel cm) {
            super();

            this.parent = cm.parent;
            this.parentSchema = cm.parentSchema;
            this.interfaces = cm.interfaces;
            this.allParents = cm.allParents;
            this.parentModel = cm.parentModel;
            this.interfaceModels = cm.interfaceModels;
            this.children = cm.children;
            this.anyOf = cm.anyOf;
            this.oneOf = cm.oneOf;
            this.allOf = cm.allOf;
            this.name = cm.name;
            this.classname = cm.classname;
            this.title = cm.title;
            this.description = cm.description;
            this.classVarName = cm.classVarName;
            this.modelJson = cm.modelJson;
            this.dataType = cm.dataType;
            this.xmlPrefix = cm.xmlPrefix;
            this.xmlNamespace = cm.xmlNamespace;
            this.xmlName = cm.xmlName;
            this.classFilename = cm.classFilename;
            this.unescapedDescription = cm.unescapedDescription;
            this.discriminator = cm.discriminator;
            this.defaultValue = cm.defaultValue;
            this.arrayModelType = cm.arrayModelType;
            this.isAlias = cm.isAlias;
            this.isString = cm.isString;
            this.isInteger = cm.isInteger;
            this.isLong = cm.isLong;
            this.isNumber = cm.isNumber;
            this.isNumeric = cm.isNumeric;
            this.isFloat = cm.isFloat;
            this.isDouble = cm.isDouble;
            this.isDate = cm.isDate;
            this.isDateTime = cm.isDateTime;
            this.vars = cm.vars;
            this.allVars = cm.allVars;
            this.requiredVars = cm.requiredVars;
            this.optionalVars = cm.optionalVars;
            this.hasReadOnly = cm.hasReadOnly;
            this.readOnlyVars = cm.readOnlyVars;
            this.readWriteVars = cm.readWriteVars;
            this.parentVars = cm.parentVars;
            this.allowableValues = cm.allowableValues;
            this.mandatory = cm.mandatory;
            this.allMandatory = cm.allMandatory;
            this.imports = cm.imports;
            this.hasVars = cm.hasVars;
            this.emptyVars = cm.emptyVars;
            this.hasMoreModels = cm.hasMoreModels;
            this.hasEnums = cm.hasEnums;
            this.isEnum = cm.isEnum;
            this.isNullable = cm.isNullable;
            this.hasRequired = cm.hasRequired;
            this.hasOptional = cm.hasOptional;
            this.isArray = cm.isArray;
            this.hasChildren = cm.hasChildren;
            this.isMap = cm.isMap;
            this.isDeprecated = cm.isDeprecated;
            this.hasOnlyReadOnly = cm.hasOnlyReadOnly;
            this.externalDocumentation = cm.externalDocumentation;

            this.vendorExtensions = cm.vendorExtensions;
            this.additionalPropertiesType = cm.additionalPropertiesType;
            this.isAdditionalPropertiesTrue = cm.isAdditionalPropertiesTrue;
            this.setMaxProperties(cm.getMaxProperties());
            this.setMinProperties(cm.getMinProperties());
            this.setUniqueItems(cm.getUniqueItems());
            this.setMaxItems(cm.getMaxItems());
            this.setMinItems(cm.getMinItems());
            this.setMaxLength(cm.getMaxLength());
            this.setMinLength(cm.getMinLength());
            this.setExclusiveMinimum(cm.getExclusiveMinimum());
            this.setExclusiveMaximum(cm.getExclusiveMaximum());
            this.setMinimum(cm.getMinimum());
            this.setMaximum(cm.getMaximum());
            this.setPattern(cm.getPattern());
            this.setMultipleOf(cm.getMultipleOf());
            this.setItems(cm.getItems());
            this.setAdditionalProperties(cm.getAdditionalProperties());
            this.setIsModel(cm.getIsModel());
        }
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiFilename(name) + ".spec";
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelFilename(name) + ".spec";
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + "apis";
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + "models";
    }
}
