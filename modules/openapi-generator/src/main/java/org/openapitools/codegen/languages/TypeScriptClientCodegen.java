/*
 * Copyright 2020 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.github.curiousoddman.rgxgen.RgxGen;
import com.google.common.collect.Sets;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE;
import org.openapitools.codegen.CodegenDiscriminator.MappedModel;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.OnceLogger.once;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;


public class TypeScriptClientCodegen extends AbstractTypeScriptClientCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(TypeScriptClientCodegen.class);

    private static final String FRAMEWORK_SWITCH = "framework";
    private static final String FRAMEWORK_SWITCH_DESC = "Specify the framework which should be used in the client code.";
    private static final String[] FRAMEWORKS = {"fetch-api", "jquery"};
    private static final String PLATFORM_SWITCH = "platform";
    private static final String PLATFORM_SWITCH_DESC = "Specifies the platform the code should run on. The default is 'node' for the 'request' framework and 'browser' otherwise.";
    private static final String[] PLATFORMS = {"browser", "node", "deno"};
    private static final String IMPORT_FILE_EXTENSION_SWITCH = "importFileExtension";
    private static final String IMPORT_FILE_EXTENSION_SWITCH_DESC = "File extension to use with relative imports. Set it to '.js' or '.mjs' when using [ESM](https://nodejs.org/api/esm.html). Defaults to '.ts' when 'platform' is set to 'deno'.";
    private static final String FILE_CONTENT_DATA_TYPE = "fileContentDataType";
    private static final String FILE_CONTENT_DATA_TYPE_DESC = "Specifies the type to use for the content of a file - i.e. Blob (Browser, Deno) / Buffer (node)";
    private static final String USE_RXJS_SWITCH = "useRxJS";
    private static final String USE_RXJS_SWITCH_DESC = "Enable this to internally use rxjs observables. If disabled, a stub is used instead. This is required for the 'angular' framework.";
    private static final String USE_INVERSIFY_SWITCH = "useInversify";
    private static final String USE_INVERSIFY_SWITCH_DESC = "Enable this to generate decorators and service identifiers for the InversifyJS inversion of control container. If you set 'deno' as 'platform', the generator will process this value as 'disable'.";

    private static final String USE_OBJECT_PARAMS_SWITCH = "useObjectParameters";
    private static final String USE_OBJECT_PARAMS_DESC = "Use aggregate parameter objects as function arguments for api operations instead of passing each parameter as a separate function argument.";

    public static final String USE_ERASABLE_SYNTAX = "useErasableSyntax";
    public static final String USE_ERASABLE_SYNTAX_DESC = "Use erasable syntax for the generated code. This is a temporary feature and will be removed in the future.";

    private final Map<String, String> frameworkToHttpLibMap;

    @Setter
    private boolean useRxJS;
    @Setter
    private boolean useInversify;

    // NPM Options
    private static final String NPM_REPOSITORY = "npmRepository";

    // NPM Option Values
    @Getter @Setter
    protected String npmRepository = null;
    protected String snapshot = null;
    protected ENUM_PROPERTY_NAMING_TYPE enumPropertyNaming = ENUM_PROPERTY_NAMING_TYPE.PascalCase;

    private final DateTimeFormatter iso8601Date = DateTimeFormatter.ISO_DATE;
    private final DateTimeFormatter iso8601DateTime = DateTimeFormatter.ISO_DATE_TIME;

    public TypeScriptClientCodegen() {
        super();

        modifyFeatureSet(features -> features.includeSecurityFeatures(SecurityFeature.BearerToken));

        this.frameworkToHttpLibMap = new HashMap<>();
        this.frameworkToHttpLibMap.put("fetch-api", "isomorphic-fetch");
        this.frameworkToHttpLibMap.put("jquery", "jquery");

        this.generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata).stability(Stability.EXPERIMENTAL).build();

        outputFolder = "generated-code" + File.separator + "typescript";
        embeddedTemplateDir = templateDir = "typescript";

        // NOTE: TypeScript uses camel cased reserved words, while models are title cased. We don't want lowercase comparisons.
        reservedWords.addAll(Arrays.asList(
                // local variable names used in API methods (endpoints)
                "from",
                // Typescript reserved words
                "constructor"));

        typeMapping.put("object", "any");
        typeMapping.put("DateTime", "Date");

        cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url your private npmRepo in the package.json"));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.FILE_CONTENT_DATA_TYPE, TypeScriptClientCodegen.FILE_CONTENT_DATA_TYPE_DESC).defaultValue("Buffer"));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.USE_RXJS_SWITCH, TypeScriptClientCodegen.USE_RXJS_SWITCH_DESC).defaultValue("false"));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.USE_OBJECT_PARAMS_SWITCH, TypeScriptClientCodegen.USE_OBJECT_PARAMS_DESC).defaultValue("false"));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.USE_INVERSIFY_SWITCH, TypeScriptClientCodegen.USE_INVERSIFY_SWITCH_DESC).defaultValue("false"));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.IMPORT_FILE_EXTENSION_SWITCH, TypeScriptClientCodegen.IMPORT_FILE_EXTENSION_SWITCH_DESC));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.USE_ERASABLE_SYNTAX, TypeScriptClientCodegen.USE_ERASABLE_SYNTAX_DESC).defaultValue("false"));

        CliOption frameworkOption = new CliOption(TypeScriptClientCodegen.FRAMEWORK_SWITCH, TypeScriptClientCodegen.FRAMEWORK_SWITCH_DESC);
        for (String option : TypeScriptClientCodegen.FRAMEWORKS) {
            frameworkOption.addEnum(option, option);
        }
        frameworkOption.defaultValue(FRAMEWORKS[0]);
        cliOptions.add(frameworkOption);

        CliOption platformOption = new CliOption(TypeScriptClientCodegen.PLATFORM_SWITCH, TypeScriptClientCodegen.PLATFORM_SWITCH_DESC);
        for (String option : TypeScriptClientCodegen.PLATFORMS) {
            platformOption.addEnum(option, option);
        }
        platformOption.defaultValue(PLATFORMS[0]);

        cliOptions.add(platformOption);

        // Set property naming to camelCase
        supportModelPropertyNaming(CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.camelCase);

        // Git
        supportingFiles.add(new SupportingFile(".gitattributes.mustache", "", ".gitattributes"));
        supportingFiles.add(new SupportingFile(".gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));

        // Util
        supportingFiles.add(new SupportingFile("util.mustache", "", "util.ts"));
        supportingFiles.add(new SupportingFile("api" + File.separator + "exception.mustache", "apis", "exception.ts"));
        // http
        supportingFiles.add(new SupportingFile("http" + File.separator + "http.mustache", "http", "http.ts"));
        supportingFiles.add(new SupportingFile("http" + File.separator + "servers.mustache", "servers.ts"));

        supportingFiles.add(new SupportingFile("configuration.mustache", "", "configuration.ts"));
        supportingFiles.add(new SupportingFile("auth" + File.separator + "auth.mustache", "auth", "auth.ts"));

        supportingFiles.add(new SupportingFile("model" + File.separator + "models_all.mustache", "models", "all.ts"));

        supportingFiles.add(new SupportingFile("types" + File.separator + "PromiseAPI.mustache", "types", "PromiseAPI.ts"));
        supportingFiles.add(new SupportingFile("types" + File.separator + "ObservableAPI.mustache", "types", "ObservableAPI.ts"));
        supportingFiles.add(new SupportingFile("types" + File.separator + "ObjectParamAPI.mustache", "types", "ObjectParamAPI.ts"));

        // models
        setModelPackage("models");
        supportingFiles.add(new SupportingFile("model" + File.separator + "ObjectSerializer.mustache", "models", "ObjectSerializer.ts"));
        modelTemplateFiles.put("model" + File.separator + "model.mustache", ".ts");

        // api
        setApiPackage("");
        supportingFiles.add(new SupportingFile("api" + File.separator + "middleware.mustache", "", "middleware.ts"));
        supportingFiles.add(new SupportingFile("api" + File.separator + "baseapi.mustache", "apis", "baseapi.ts"));
        apiTemplateFiles.put("api" + File.separator + "api.mustache", ".ts");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        final Object propFramework = additionalProperties.get(FRAMEWORK_SWITCH);

        Map<String, Boolean> frameworks = new HashMap<>();
        for (String framework : FRAMEWORKS) {
            frameworks.put(framework, framework.equals(propFramework));
        }
        objs.put("framework", propFramework);
        objs.put("frameworks", frameworks);

        objs.put("fileContentDataType", additionalProperties.get(FILE_CONTENT_DATA_TYPE));

        return objs;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap operations, List<ModelMap> models) {

        // Add additional filename information for model imports in the apis
        List<Map<String, String>> imports = operations.getImports();
        for (Map<String, String> im : imports) {
            im.put("filename", im.get("import"));
        }

        OperationMap operationsMap = operations.getOperations();
        List<CodegenOperation> operationList = operationsMap.getOperation();
        for (CodegenOperation operation : operationList) {
            List<CodegenResponse> responses = operation.responses;
            operation.returnType = this.getReturnType(responses);
        }
        return operations;
    }

    /**
     * Returns the correct return type based on all 2xx HTTP responses defined for an operation.
     *
     * @param responses all CodegenResponses defined for one operation
     * @return TypeScript return type
     */
    private String getReturnType(List<CodegenResponse> responses) {
        Set<String> returnTypes = new HashSet<>();
        for (CodegenResponse response : responses) {
            if (response.is2xx) {
                if (response.dataType != null) {
                    returnTypes.add(response.dataType);
                } else {
                    returnTypes.add("void");
                }
            }
        }

        if (returnTypes.size() == 0) {
            return null;
        }

        return String.join(" | ", returnTypes);
    }

    @Override
    public String toParamName(String name) {
        // sanitize name
        name = sanitizeName(name);

        if ("_".equals(name)) {
            name = "_u";
        }

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        return super.toParamName(name);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        if ("_".equals(name)) {
            name = "_u";
        }

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        return super.toVarName(name);
    }

    @Override
    public String toModelImport(String name) {
        // Use `/` instead of `File.Separator`. `File.Separator` is `\` in Windows, which is invalid Typescript.
        return "../" + modelPackage() + "/" + toModelName(name);
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("number".equals(datatype)) {
            return value;
        } else {
            return "\'" + escapeText(value) + "\'";
        }
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return getNameWithEnumPropertyNaming("empty");
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return getNameWithEnumPropertyNaming(getSymbolName(name));
        }

        // number
        if ("number".equals(datatype)) {
            String varName = "NUMBER_" + name;

            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String enumName = sanitizeName(name);
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        enumName = getNameWithEnumPropertyNaming(enumName);

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        }

        if (enumName.isEmpty()) {
            // After sanitizing *all* characters (e.g. multibyte characters), the var name becomes an empty string.
            // An empty string would cause a syntax error, so this code attempts to re-sanitize the name using another sanitizer that allows a wider variety of characters.
            // For backward compatibility, this additional sanitization is only applied if the original sanitized name is empty.
            final String sanitized = sanitizeNameForTypeScriptSymbol(name);
            if (sanitized.isEmpty()) {
                // After re-sanitizing, this pads a pseudo var name ("STRING") if still the name is empty.
                return "STRING";
            }
            return "_" + sanitized;
        }

        return enumName;
    }

    private String sanitizeNameForTypeScriptSymbol(String name) {
        return sanitizeName(name, "[^\\p{L}\\p{Nd}\\$_]");
    }

    private String getNameWithEnumPropertyNaming(String name) {
        switch (getEnumPropertyNaming()) {
            case original:
                return name;
            case camelCase:
                return camelize(underscore(name), LOWERCASE_FIRST_LETTER);
            case PascalCase:
                return camelize(underscore(name));
            case snake_case:
                return underscore(name);
            case UPPERCASE:
                return underscore(name).toUpperCase(Locale.ROOT);
            default:
                throw new IllegalArgumentException("Unsupported enum property naming: '" + name);
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // process enum in models
        List<ModelMap> models = postProcessModelsEnum(objs).getModels();
        for (ModelMap mo : models) {
            CodegenModel cm = mo.getModel();
            cm.imports = new TreeSet<>(cm.imports);
            // name enum with model name, e.g. StatusEnum => Pet.StatusEnum
            for (CodegenProperty var : cm.vars) {
                if (Boolean.TRUE.equals(var.isEnum)) {
                    var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
                }
            }
            if (cm.parent != null) {
                for (CodegenProperty var : cm.allVars) {
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
        }
        for (ModelMap mo : models) {
            CodegenModel cm = mo.getModel();
            // Add additional filename information for imports
            mo.put("tsImports", toTsImports(cm, cm.imports));
        }
        return objs;
    }

    private List<Map<String, String>> toTsImports(CodegenModel cm, Set<String> imports) {
        List<Map<String, String>> tsImports = new ArrayList<>();
        for (String im : imports) {
            if (!im.equals(cm.classname)) {
                HashMap<String, String> tsImport = new HashMap<>();
                // TVG: This is used as class name in the import statements of the model file
                tsImport.put("classname", im);
                tsImport.put("filename", importMapping.getOrDefault(im, toModelImport(im)));
                tsImports.add(tsImport);
            }
        }
        return tsImports;
    }

    @Override
    public String getName() {
        return "typescript";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript client library using Fetch API (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // change package names
        apiPackage = this.apiPackage + ".apis";
        testPackage = this.testPackage + ".tests";

        additionalProperties.putIfAbsent(FRAMEWORK_SWITCH, FRAMEWORKS[0]);
        supportingFiles.add(new SupportingFile("index.mustache", "index.ts"));

        String httpLibName = this.getHttpLibForFramework(additionalProperties.get(FRAMEWORK_SWITCH).toString());
        supportingFiles.add(new SupportingFile(
                "http" + File.separator + httpLibName + ".mustache",
                "http", httpLibName + ".ts"
        ));

        Object propPlatform = additionalProperties.get(PLATFORM_SWITCH);
        if (propPlatform == null) {
            propPlatform = "browser";
            additionalProperties.put("platform", propPlatform);
        }

        Map<String, Boolean> platforms = new HashMap<>();
        for (String platform : PLATFORMS) {
            platforms.put(platform, platform.equals(propPlatform));
        }
        additionalProperties.put("platforms", platforms);

        additionalProperties.putIfAbsent(FILE_CONTENT_DATA_TYPE, "node".equals(propPlatform) ? "Buffer" : "Blob");

        if (!"deno".equals(propPlatform)) {
            supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
            supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
            supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        }

        Object fileExtension = additionalProperties.get(IMPORT_FILE_EXTENSION_SWITCH);
        if (fileExtension == null && "deno".equals(propPlatform)) {
            additionalProperties.put(IMPORT_FILE_EXTENSION_SWITCH, ".ts");
        }

        convertPropertyToBooleanAndWriteBack(USE_RXJS_SWITCH, this::setUseRxJS);
        if (!useRxJS) {
            supportingFiles.add(new SupportingFile("rxjsStub.mustache", "rxjsStub.ts"));
        }

        convertPropertyToBooleanAndWriteBack(USE_INVERSIFY_SWITCH, this::setUseInversify);
        if (useInversify) {
            supportingFiles.add(new SupportingFile("services" + File.separator + "index.mustache", "services", "index.ts"));
            supportingFiles.add(new SupportingFile("services" + File.separator + "configuration.mustache", "services", "configuration.ts"));
            supportingFiles.add(new SupportingFile("services" + File.separator + "PromiseAPI.mustache", "services", "PromiseAPI.ts"));
            supportingFiles.add(new SupportingFile("services" + File.separator + "ObservableAPI.mustache", "services", "ObservableAPI.ts"));
            supportingFiles.add(new SupportingFile("services" + File.separator + "ObjectParamAPI.mustache", "services", "ObjectParamAPI.ts"));
            supportingFiles.add(new SupportingFile("services" + File.separator + "http.mustache", "services", "http.ts"));
            apiTemplateFiles.put("services" + File.separator + "api.mustache", ".service.ts");
        }

        // NPM Settings
        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }
    }

    private String getHttpLibForFramework(String object) {
        return this.frameworkToHttpLibMap.get(object);
    }


    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            Schema<?> inner = getSchemaAdditionalProperties(p);
            String postfix = "";
            if (Boolean.TRUE.equals(inner.getNullable())) {
                postfix = " | null";
            }
            return "{ [key: string]: " + this.getTypeDeclaration(unaliasSchema(inner)) + postfix + "; }";
        } else if (ModelUtils.isFileSchema(p)) {
            return "HttpFile";
        } else if (ModelUtils.isBinarySchema(p)) {
            return "any";
        } else {
            return super.getTypeDeclaration(p);
        }
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        codegenModel.additionalPropertiesType = getSchemaType(ModelUtils.getAdditionalProperties(schema));
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }

    public String typescriptDate(Object dateValue) {
        String strValue = null;
        if (dateValue instanceof OffsetDateTime) {
            OffsetDateTime date = null;
            try {
                date = (OffsetDateTime) dateValue;
            } catch (ClassCastException e) {
                LOGGER.warn("Invalid `date` format for value {}", dateValue);
                date = ((Date) dateValue).toInstant().atOffset(ZoneOffset.UTC);
            }
            strValue = date.format(iso8601Date);
        } else {
            strValue = dateValue.toString();
        }
        return "new Date('" + strValue + "').toISOString().split('T')[0];";
    }

    public String typescriptDateTime(Object dateTimeValue) {
        String strValue = null;
        if (dateTimeValue instanceof OffsetDateTime) {
            OffsetDateTime dateTime = null;
            try {
                dateTime = (OffsetDateTime) dateTimeValue;
            } catch (ClassCastException e) {
                LOGGER.warn("Invalid `date-time` format for value {}", dateTimeValue);
                dateTime = ((Date) dateTimeValue).toInstant().atOffset(ZoneOffset.UTC);
            }
            strValue = dateTime.format(iso8601DateTime);
        } else {
            strValue = dateTimeValue.toString();
        }
        return "new Date('" + strValue + "')";
    }

    public String getModelName(Schema sc) {
        if (sc.get$ref() != null) {
            Schema unaliasedSchema = unaliasSchema(sc);
            if (unaliasedSchema.get$ref() != null) {
                return toModelName(ModelUtils.getSimpleRef(sc.get$ref()));
            }
        }
        return null;
    }

    /**
     * Gets an example if it exists
     *
     * @param sc input schema
     * @return the example value
     */
    protected Object getObjectExample(Schema sc) {
        Schema schema = sc;
        String ref = sc.get$ref();
        if (ref != null) {
            schema = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
        }
        // TODO handle examples in object models in the future
        Boolean objectModel = (ModelUtils.isObjectSchema(schema) || ModelUtils.isMapSchema(schema) || ModelUtils.isComposedSchema(schema));
        if (objectModel) {
            return null;
        }
        if (schema.getExample() != null) {
            return schema.getExample();
        }
        if (schema.getDefault() != null) {
            return schema.getDefault();
        } else if (schema.getEnum() != null && !schema.getEnum().isEmpty()) {
            return schema.getEnum().get(0);
        }
        return null;
    }

    /***
     * Ensures that the string has a leading and trailing quote
     *
     * @param in input string
     * @return quoted string
     */
    private String ensureQuotes(String in) {
        Pattern pattern = Pattern.compile("\r\n|\r|\n");
        Matcher matcher = pattern.matcher(in);
        if (matcher.find()) {
            // if a string has a new line in it add backticks to make it a typescript multiline string
            return "`" + in + "`";
        }
        String strPattern = "^['\"].*?['\"]$";
        if (in.matches(strPattern)) {
            return in;
        }
        return "\"" + in + "\"";
    }

    @Override
    public String toExampleValue(Schema schema) {
        Object objExample = getObjectExample(schema);
        return toExampleValue(schema, objExample);
    }

    public String toExampleValue(Schema schema, Object objExample) {
        String modelName = getModelName(schema);
        return toExampleValueRecursive(modelName, schema, objExample, 1, "", 0, Sets.newHashSet());
    }

    private Boolean simpleStringSchema(Schema schema) {
        Schema sc = schema;
        String ref = schema.get$ref();
        if (ref != null) {
            sc = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
        }
        return ModelUtils.isStringSchema(sc) && !ModelUtils.isDateSchema(sc) && !ModelUtils.isDateTimeSchema(sc) && !"Number".equalsIgnoreCase(sc.getFormat()) && !ModelUtils.isByteArraySchema(sc) && !ModelUtils.isBinarySchema(sc) && schema.getPattern() == null;
    }

    private MappedModel getDiscriminatorMappedModel(CodegenDiscriminator disc) {
        for (MappedModel mm : disc.getMappedModels()) {
            String modelName = mm.getModelName();
            Schema modelSchema = getModelNameToSchemaCache().get(modelName);
            if (ModelUtils.isObjectSchema(modelSchema)) {
                return mm;
            }
        }
        return null;
    }

    /***
     * Recursively generates string examples for schemas
     *
     * @param modelName the string name of the refed model that will be generated for the schema or null
     * @param schema the schema that we need an example for
     * @param objExample the example that applies to this schema, for now only string example are used
     * @param indentationLevel integer indentation level that we are currently at
     *                         we assume the indentation amount is 2 spaces times this integer
     * @param prefix the string prefix that we will use when assigning an example for this line
     *               this is used when setting key: value, pairs "key: " is the prefix
     *               and this is used when setting properties like some_property='some_property_example'
     * @param exampleLine this is the current line that we are generating an example for, starts at 0
     *                    we don't indent the 0th line because using the example value looks like:
     *                    prop = ModelName( line 0
     *                        some_property='some_property_example' line 1
     *                    ) line 2
     *                    and our example value is:
     *                    ModelName( line 0
     *                        some_property='some_property_example' line 1
     *                    ) line 2
     * @param seenSchemas This set contains all the schemas passed into the recursive function. It is used to check
     *                    if a schema was already passed into the function and breaks the infinite recursive loop. The
     *                    only schemas that are not added are ones that contain $ref != null
     * @return the string example
     */
    private String toExampleValueRecursive(String modelName, Schema schema, Object objExample, int indentationLevel, String prefix, Integer exampleLine, Set<Schema> seenSchemas) {
        final String indentionConst = "  ";
        String currentIndentation = "";
        String closingIndentation = "";
        for (int i = 0; i < indentationLevel; i++) currentIndentation += indentionConst;
        if (exampleLine.equals(0)) {
            closingIndentation = currentIndentation;
            currentIndentation = "";
        } else {
            closingIndentation = currentIndentation;
        }
        String openChars = "";
        String closeChars = "";
        String fullPrefix = currentIndentation + prefix + openChars;

        String example = null;
        if (objExample != null) {
            example = objExample.toString();
        }
        // checks if the current schema has already been passed in. If so, breaks the current recursive pass
        if (seenSchemas.contains(schema)) {
            if (modelName != null) {
                return fullPrefix + closeChars;
            } else {
                // this is a recursive schema
                // need to add a reasonable example to avoid
                // infinite recursion
                if (ModelUtils.isNullable(schema)) {
                    // if the schema is nullable, then 'null' is a valid value
                    return fullPrefix + "null" + closeChars;
                } else if (ModelUtils.isArraySchema(schema)) {
                    // the schema is an array, add an empty array
                    return fullPrefix + "[]" + closeChars;
                } else {
                    // the schema is an object, make an empty object
                    return fullPrefix + "{}" + closeChars;
                }
            }
        }

        if (null != schema.get$ref()) {
            Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
            String ref = ModelUtils.getSimpleRef(schema.get$ref());
            Schema refSchema = allDefinitions.get(ref);
            if (null == refSchema) {
                LOGGER.warn("Unable to find referenced schema " + schema.get$ref() + "\n");
                return fullPrefix + "null" + closeChars;
            }
            String refModelName = getModelName(schema);
            return toExampleValueRecursive(refModelName, refSchema, objExample, indentationLevel, prefix, exampleLine, seenSchemas);
        } else if (ModelUtils.isNullType(schema) || ModelUtils.isAnyType(schema)) {
            // The 'null' type is allowed in OAS 3.1 and above. It is not supported by OAS 3.0.x,
            // though this tooling supports it.
            return fullPrefix + "null" + closeChars;
        } else if (ModelUtils.isBooleanSchema(schema)) {
            if (objExample == null) {
                example = "true";
            } else {
                if ("false".equalsIgnoreCase(objExample.toString())) {
                    example = "false";
                } else {
                    example = "true";
                }
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isDateSchema(schema)) {
            if (objExample == null) {
                example = typescriptDate("1970-01-01");
            } else {
                example = typescriptDate(objExample);
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            if (objExample == null) {
                example = typescriptDateTime("1970-01-01T00:00:00.00Z");
            } else {
                example = typescriptDateTime(objExample);
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isBinarySchema(schema)) {
            if (objExample == null) {
                example = "/path/to/file";
            }
            example = "{ data: Buffer.from(fs.readFileSync('" + example + "', 'utf-8')), name: '" + example + "' }";
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isByteArraySchema(schema)) {
            if (objExample == null) {
                example = "'YQ=='";
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isStringSchema(schema)) {
            if (objExample == null) {
                // a BigDecimal:
                if ("Number".equalsIgnoreCase(schema.getFormat())) {
                    example = "2";
                    return fullPrefix + example + closeChars;
                } else if (StringUtils.isNotBlank(schema.getPattern())) {
                    String pattern = schema.getPattern();
                    RgxGen rgxGen = new RgxGen(pattern);

                    // this seed makes it so if we have [a-z] we pick a
                    Random random = new Random(18);
                    example = rgxGen.generate(random);
                } else if (schema.getMinLength() != null) {
                    example = "";
                    int len = schema.getMinLength().intValue();
                    for (int i = 0; i < len; i++) example += "a";
                } else if (ModelUtils.isUUIDSchema(schema)) {
                    example = "046b6c7f-0b8a-43b9-b35d-6489e6daee91";
                } else {
                    example = "string_example";
                }
            }
            return fullPrefix + ensureQuotes(example) + closeChars;
        } else if (ModelUtils.isIntegerSchema(schema)) {
            if (objExample == null) {
                if (schema.getMinimum() != null) {
                    example = schema.getMinimum().toString();
                } else {
                    example = "1";
                }
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isNumberSchema(schema)) {
            if (objExample == null) {
                if (schema.getMinimum() != null) {
                    example = schema.getMinimum().toString();
                } else {
                    example = "3.14";
                }
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isArraySchema(schema)) {
            Schema itemSchema = ModelUtils.getSchemaItems(schema);
            String itemModelName = getModelName(itemSchema);
            if (objExample instanceof Iterable && itemModelName == null) {
                // If the example is already a list, return it directly instead of wrongly wrap it in another list
                return fullPrefix + objExample + closeChars;
            }
            Set<Schema> newSeenSchemas = new HashSet<>(seenSchemas);
            newSeenSchemas.add(schema);
            example = fullPrefix + "[" + "\n" + toExampleValueRecursive(itemModelName, itemSchema, objExample, indentationLevel + 1, "", exampleLine + 1, newSeenSchemas) + ",\n" + closingIndentation + "]" + closeChars;
            return example;
        } else if (ModelUtils.isMapSchema(schema)) {
            if (modelName == null) {
                fullPrefix += "{";
                closeChars = "}";
            }
            Object addPropsObj = schema.getAdditionalProperties();
            // TODO handle true case for additionalProperties
            if (addPropsObj instanceof Schema) {
                Schema addPropsSchema = (Schema) addPropsObj;
                String key = "key";
                Object addPropsExample = getObjectExample(addPropsSchema);
                if (addPropsSchema.getEnum() != null && !addPropsSchema.getEnum().isEmpty()) {
                    key = addPropsSchema.getEnum().get(0).toString();
                }
                addPropsExample = exampleFromStringOrArraySchema(addPropsSchema, addPropsExample, key);
                String addPropPrefix = key + ": ";
                if (modelName == null) {
                    addPropPrefix = ensureQuotes(key) + ": ";
                }
                String addPropsModelName = "\"" + getModelName(addPropsSchema) + "\"";
                Set<Schema> newSeenSchemas = new HashSet<>(seenSchemas);
                newSeenSchemas.add(schema);
                example = fullPrefix + "\n" + toExampleValueRecursive(addPropsModelName, addPropsSchema, addPropsExample, indentationLevel + 1, addPropPrefix, exampleLine + 1, newSeenSchemas) + ",\n" + closingIndentation + closeChars;
            } else {
                example = fullPrefix + closeChars;
            }
            return example;
        } else if (ModelUtils.isComposedSchema(schema)) {
            ComposedSchema cm = (ComposedSchema) schema;
            List<Schema> ls = cm.getOneOf();
            if (ls != null && !ls.isEmpty()) {
                return fullPrefix + toExampleValue(ls.get(0)) + closeChars;
            }
            return fullPrefix + closeChars;
        } else if (ModelUtils.isObjectSchema(schema)) {
            fullPrefix += "{";
            closeChars = "}";
            CodegenDiscriminator disc = createDiscriminator(modelName, schema);
            if (disc != null) {
                MappedModel mm = getDiscriminatorMappedModel(disc);
                if (mm != null) {
                    String discPropNameValue = mm.getMappingName();
                    String chosenModelName = mm.getModelName();
                    // TODO handle this case in the future, this is when the discriminated
                    // schema allOf includes this schema, like Cat allOf includes Pet
                    // so this is the composed schema use case
                } else {
                    return fullPrefix + closeChars;
                }
            }

            Set<Schema> newSeenSchemas = new HashSet<>(seenSchemas);
            newSeenSchemas.add(schema);
            String exampleForObjectModel = exampleForObjectModel(schema, fullPrefix, closeChars, null, indentationLevel, exampleLine, closingIndentation, newSeenSchemas);
            return exampleForObjectModel;
        } else {
            LOGGER.warn("Type " + schema.getType() + " not handled properly in toExampleValue");
        }

        return example;
    }

    private String exampleForObjectModel(Schema schema, String fullPrefix, String closeChars, CodegenProperty discProp, int indentationLevel, int exampleLine, String closingIndentation, Set<Schema> seenSchemas) {
        Map<String, Schema> requiredAndOptionalProps = schema.getProperties();
        if (requiredAndOptionalProps == null || requiredAndOptionalProps.isEmpty()) {
            return fullPrefix + closeChars;
        }

        String example = fullPrefix + "\n";
        for (Map.Entry<String, Schema> entry : requiredAndOptionalProps.entrySet()) {
            String propName = entry.getKey();
            Schema propSchema = entry.getValue();
            boolean readOnly = false;
            if (propSchema.getReadOnly() != null) {
                readOnly = propSchema.getReadOnly();
            }
            if (readOnly) {
                continue;
            }
            String ref = propSchema.get$ref();
            if (ref != null) {
                Schema refSchema = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
                if (refSchema.getReadOnly() != null) {
                    readOnly = refSchema.getReadOnly();
                }
                if (readOnly) {
                    continue;
                }
            }
            propName = toVarName(propName);
            String propModelName = null;
            Object propExample = null;
            if (discProp != null && propName.equals(discProp.name)) {
                propModelName = null;
                propExample = discProp.example;
            } else {
                propModelName = getModelName(propSchema);
                propExample = exampleFromStringOrArraySchema(propSchema, null, propName);
            }
            example += toExampleValueRecursive(propModelName, propSchema, propExample, indentationLevel + 1, propName + ": ", exampleLine + 1, seenSchemas) + ",\n";
        }
        // TODO handle additionalProperties also
        example += closingIndentation + closeChars;
        return example;
    }

    private Object exampleFromStringOrArraySchema(Schema sc, Object currentExample, String propName) {
        if (currentExample != null) {
            return currentExample;
        }
        Schema schema = sc;
        String ref = sc.get$ref();
        if (ref != null) {
            schema = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
        }
        Object example = getObjectExample(schema);
        if (example != null) {
            return example;
        } else if (simpleStringSchema(schema)) {
            return propName + "_example";
        } else if (ModelUtils.isArraySchema(schema)) {
            Schema itemSchema = ModelUtils.getSchemaItems(schema);
            example = getObjectExample(itemSchema);
            if (example != null) {
                return example;
            } else if (simpleStringSchema(itemSchema)) {
                return propName + "_example";
            }
        }
        return null;
    }

    protected String setPropertyExampleValue(CodegenProperty p) {
        String example;

        if (p == null) {
            return "null";
        }

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            example = p.defaultValue;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if (Boolean.TRUE.equals(p.isInteger)) {
            if (example == null) {
                example = "56";
            }
        } else if (Boolean.TRUE.equals(p.isLong)) {
            if (example == null) {
                example = "789";
            }
        } else if (Boolean.TRUE.equals(p.isDouble)
                || Boolean.TRUE.equals(p.isFloat)
                || Boolean.TRUE.equals(p.isNumber)) {
            if (example == null) {
                example = "3.4";
            }
        } else if (Boolean.TRUE.equals(p.isBoolean)) {
            if (example == null) {
                example = "true";
            }
        } else if (Boolean.TRUE.equals(p.isFile) || Boolean.TRUE.equals(p.isBinary)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if (Boolean.TRUE.equals(p.isDate)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "new Date(\"" + escapeText(example) + "\")";
        } else if (Boolean.TRUE.equals(p.isDateTime)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "new Date(\"" + escapeText(example) + "\")";
        } else if (Boolean.TRUE.equals(p.isString)) {
            if (example == null) {
                example = p.name + "_example";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = "new " + "{{moduleName}}" + "." + type + "()";
        }

        return example;
    }

    /***
     * Set the codegenParameter example value
     * We have a custom version of this function so we can invoke toExampleValue
     *
     * @param codegenParameter the item we are setting the example on
     * @param parameter the base parameter that came from the spec
     */
    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter, Parameter parameter) {
        Schema schema = parameter.getSchema();
        if (schema == null) {
            LOGGER.warn("CodegenParameter.example defaulting to null because parameter lacks a schema");
            return;
        }

        Object example = null;
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            example = codegenParameter.vendorExtensions.get("x-example");
        } else if (parameter.getExample() != null) {
            example = parameter.getExample();
        } else if (parameter.getExamples() != null && !parameter.getExamples().isEmpty() && parameter.getExamples().values().iterator().next().getValue() != null) {
            example = parameter.getExamples().values().iterator().next().getValue();
        } else {
            example = getObjectExample(schema);
        }
        example = exampleFromStringOrArraySchema(schema, example, parameter.getName());
        String finalExample = toExampleValue(schema, example);
        codegenParameter.example = finalExample;
    }

    /**
     * Return the example value of the parameter.
     *
     * @param codegenParameter Codegen parameter
     * @param requestBody      Request body
     */
    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter, RequestBody requestBody) {
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            codegenParameter.example = Json.pretty(codegenParameter.vendorExtensions.get("x-example"));
        }

        Content content = requestBody.getContent();

        if (content.size() > 1) {
            // @see ModelUtils.getSchemaFromContent()
            once(LOGGER).debug("Multiple MediaTypes found, using only the first one");
        }

        MediaType mediaType = content.values().iterator().next();
        Schema schema = mediaType.getSchema();
        if (schema == null) {
            LOGGER.warn("CodegenParameter.example defaulting to null because requestBody content lacks a schema");
            return;
        }

        Object example = null;
        if (mediaType.getExample() != null) {
            example = mediaType.getExample();
        } else if (mediaType.getExamples() != null && !mediaType.getExamples().isEmpty() && mediaType.getExamples().values().iterator().next().getValue() != null) {
            example = mediaType.getExamples().values().iterator().next().getValue();
        } else {
            example = getObjectExample(schema);
        }
        example = exampleFromStringOrArraySchema(schema, example, codegenParameter.paramName);
        codegenParameter.example = toExampleValue(schema, example);
    }

    /**
     * Create a CodegenParameter for a Form Property
     * We have a custom version of this method so we can invoke
     * setParameterExampleValue(codegenParameter, parameter)
     * rather than setParameterExampleValue(codegenParameter)
     * This ensures that all of our samples are generated in
     * toExampleValueRecursive
     *
     * @param name           the property name
     * @param propertySchema the property schema
     * @param imports        our import set
     * @return the resultant CodegenParameter
     */
    @Override
    public CodegenParameter fromFormProperty(String name, Schema propertySchema, Set<String> imports) {
        CodegenParameter cp = super.fromFormProperty(name, propertySchema, imports);
        Parameter p = new Parameter();
        p.setSchema(propertySchema);
        p.setName(cp.paramName);
        setParameterExampleValue(cp, p);
        return cp;
    }

    @Override
    protected void addImport(Set<String> importsToBeAddedTo, String type) {
        if (type == null) {
            return;
        }

        String[] parts = splitComposedTypes(type);
        for (String s : parts) {
            super.addImport(importsToBeAddedTo, s);
        }
    }

    /**
     * Split composed types
     * e.g. TheFirstType | TheSecondType to TheFirstType and TheSecondType
     *
     * @param type String with composed types
     * @return list of types
     */
    protected String[] splitComposedTypes(String type) {
        return type.replace(" ", "").split("[|&<>]");
    }
}
