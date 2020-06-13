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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.security.SecurityScheme;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;


public class TypeScriptClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(TypeScriptClientCodegen.class);

    private static final String X_DISCRIMINATOR_TYPE = "x-discriminator-value";
    private static final String UNDEFINED_VALUE = "undefined";

    private static final String FRAMEWORK_SWITCH = "framework";
    private static final String FRAMEWORK_SWITCH_DESC = "Specify the framework which should be used in the client code.";
    private static final String[] FRAMEWORKS = { "fetch-api", "jquery" };
    private static final String PLATFORM_SWITCH = "platform";
    private static final String PLATFORM_SWITCH_DESC = "Specifies the platform the code should run on. The default is 'node' for the 'request' framework and 'browser' otherwise.";
    private static final String[] PLATFORMS = { "browser", "node" };
    private static final String FILE_CONTENT_DATA_TYPE= "fileContentDataType";
    private static final String FILE_CONTENT_DATA_TYPE_DESC = "Specifies the type to use for the content of a file - i.e. Blob (Browser) / Buffer (node)";
    private static final String USE_RXJS_SWITCH = "useRxJS";
    private static final String USE_RXJS_SWITCH_DESC = "Enable this to internally use rxjs observables. If disabled, a stub is used instead. This is required for the 'angular' framework.";
    private static final String USE_INVERSIFY_SWITCH = "useInversify";
    private static final String USE_INVERSIFY_SWITCH_DESC = "Enable this to generate decorators and service identifiers for the InversifyJS inversion of control container.";

    private static final String USE_OBJECT_PARAMS_SWITCH = "useObjectParameters";
    private static final String USE_OBJECT_PARAMS_DESC = "Use aggregate parameter objects as function arguments for api operations instead of passing each parameter as a separate function argument.";

    
    private final Map<String, String> frameworkToHttpLibMap;
    
    // NPM Options
    private static final String SNAPSHOT = "snapshot";
    @SuppressWarnings("squid:S5164")
    protected static final ThreadLocal<SimpleDateFormat> SNAPSHOT_SUFFIX_FORMAT = ThreadLocal.withInitial(() -> new SimpleDateFormat("yyyyMMddHHmm", Locale.ROOT));
    private static final String NPM_REPOSITORY = "npmRepository";
    private static final String NPM_NAME = "npmName";
    private static final String NPM_VERSION = "npmVersion";

    // NPM Option Values
    protected String npmRepository = null;
    protected String snapshot = null;
    protected String npmName = null;
    protected String npmVersion = "1.0.0";
    protected String modelPropertyNaming = "camelCase";
    protected HashSet<String> languageGenericTypes;

    public TypeScriptClientCodegen() {
        super();
            
        this.frameworkToHttpLibMap = new HashMap<>();
        this.frameworkToHttpLibMap.put("fetch-api", "isomorphic-fetch");
        this.frameworkToHttpLibMap.put("jquery", "jquery");
        
        
        this.generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata).stability(Stability.EXPERIMENTAL).build();
        
        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();
        outputFolder = "generated-code" + File.separator + "typescript";
        embeddedTemplateDir = templateDir = "typescript";

        supportsInheritance = true;
        
        // NOTE: TypeScript uses camel cased reserved words, while models are title cased. We don't want lowercase comparisons.
        reservedWords.addAll(Arrays.asList(
                // local variable names used in API methods (endpoints)
                "varLocalPath", "queryParameters", "headerParams", "formParams", "useFormData", "varLocalDeferred",
                "requestOptions",
                // Typescript reserved words
                "abstract", "await", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "debugger", "default", "delete", "do", "double", "else", "enum", "export", "extends", "false", "final", "finally", "float", "for", "function", "goto", "if", "implements", "import", "in", "instanceof", "int", "interface", "let", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "super", "switch", "synchronized", "this", "throw", "transient", "true", "try", "typeof", "var", "void", "volatile", "while", "with", "yield"));

        languageSpecificPrimitives = new HashSet<>(Arrays.asList(
                "string",
                "String",
                "boolean",
                "Boolean",
                "Double",
                "Integer",
                "Long",
                "Float",
                "Object",
                "Array",
                "Date",
                "number",
                "any",
                "File",
                "Error",
                "Map"
        ));

        languageGenericTypes = new HashSet<String>(Arrays.asList(
                "Array"
        ));

        instantiationTypes.put("array", "Array");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("Array", "Array");
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "number");
        typeMapping.put("float", "number");
        typeMapping.put("number", "number");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("char", "string");
        typeMapping.put("double", "number");
        typeMapping.put("object", "any");
        typeMapping.put("integer", "number");
        typeMapping.put("Map", "any");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("binary", "any");
        typeMapping.put("File", "any");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("Error", "Error");
                

        cliOptions.add(new CliOption(NPM_NAME, "The name under which you want to publish generated npm package." +
                " Required to generate a full package"));
        cliOptions.add(new CliOption(NPM_VERSION, "The version of your npm package. If not provided, using the version from the OpenAPI specification file.").defaultValue(this.getNpmVersion()));
        cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url your private npmRepo in the package.json"));
        cliOptions.add(CliOption.newBoolean(SNAPSHOT,
                "When setting this property to true, the version will be suffixed with -SNAPSHOT." + this.SNAPSHOT_SUFFIX_FORMAT.get().toPattern(),
                false));

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue("camelCase"));
        cliOptions.add(new CliOption(CodegenConstants.SUPPORTS_ES6, CodegenConstants.SUPPORTS_ES6_DESC).defaultValue("false"));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.FILE_CONTENT_DATA_TYPE, TypeScriptClientCodegen.FILE_CONTENT_DATA_TYPE_DESC).defaultValue("Buffer"));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.USE_RXJS_SWITCH, TypeScriptClientCodegen.USE_RXJS_SWITCH_DESC).defaultValue("false"));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.USE_OBJECT_PARAMS_SWITCH, TypeScriptClientCodegen.USE_OBJECT_PARAMS_DESC).defaultValue("false"));
        cliOptions.add(new CliOption(TypeScriptClientCodegen.USE_INVERSIFY_SWITCH, TypeScriptClientCodegen.USE_INVERSIFY_SWITCH_DESC).defaultValue("false"));

        CliOption frameworkOption = new CliOption(TypeScriptClientCodegen.FRAMEWORK_SWITCH, TypeScriptClientCodegen.FRAMEWORK_SWITCH_DESC);
        for (String option: TypeScriptClientCodegen.FRAMEWORKS) {
            frameworkOption.addEnum(option, option);
        }
        frameworkOption.defaultValue(FRAMEWORKS[0]);
        cliOptions.add(frameworkOption);

        CliOption platformOption = new CliOption(TypeScriptClientCodegen.PLATFORM_SWITCH, TypeScriptClientCodegen.PLATFORM_SWITCH_DESC);
        for (String option: TypeScriptClientCodegen.PLATFORMS) {
            platformOption.addEnum(option, option);
        }
        platformOption.defaultValue(PLATFORMS[0]);

        cliOptions.add(platformOption);
        
        //Documentation
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
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
        setModelPackage("");
        supportingFiles.add(new SupportingFile("model" + File.separator + "ObjectSerializer.mustache", "models", "ObjectSerializer.ts"));
        modelTemplateFiles.put("model" + File.separator + "model.mustache", ".ts");

        // api
        setApiPackage("");
        supportingFiles.add(new SupportingFile("api" + File.separator + "middleware.mustache", "", "middleware.ts"));
        supportingFiles.add(new SupportingFile("api" + File.separator + "baseapi.mustache", "apis", "baseapi.ts"));
        apiTemplateFiles.put("api" + File.separator + "api.mustache", ".ts");
    }

    public String getNpmName() {
        return npmName;
    }

    public void setNpmName(String npmName) {
        this.npmName = npmName;
    }

    public String getNpmRepository() {
        return npmRepository;
    }

    public void setNpmRepository(String npmRepository) {
        this.npmRepository = npmRepository;
    }

    public String getNpmVersion() {
        return npmVersion;
    }

    public void setNpmVersion(String npmVersion) {
        this.npmVersion = npmVersion;
    }
    
    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }
    
    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {

        if (additionalProperties.containsKey(NPM_NAME)) {

            // If no npmVersion is provided in additional properties, version from API specification is used.
            // If none of them is provided then fallbacks to default version
            if (additionalProperties.containsKey(NPM_VERSION)) {
                this.setNpmVersion(additionalProperties.get(NPM_VERSION).toString());
            } else if (openAPI.getInfo() != null && openAPI.getInfo().getVersion() != null) {
                this.setNpmVersion(openAPI.getInfo().getVersion());
            }

            if (additionalProperties.containsKey(SNAPSHOT) && Boolean.parseBoolean(additionalProperties.get(SNAPSHOT).toString())) {
                if (npmVersion.toUpperCase(Locale.ROOT).matches("^.*-SNAPSHOT$")) {
                    this.setNpmVersion(npmVersion + "." + SNAPSHOT_SUFFIX_FORMAT.get().format(new Date()));
                } else {
                    this.setNpmVersion(npmVersion + "-SNAPSHOT." + SNAPSHOT_SUFFIX_FORMAT.get().format(new Date()));
                }
            }
            additionalProperties.put(NPM_VERSION, npmVersion);

        }
    }
    
    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {      
        final Object propFramework = additionalProperties.get(FRAMEWORK_SWITCH);

        Map<String, Boolean> frameworks = new HashMap<>();
        for (String framework: FRAMEWORKS) {
            frameworks.put(framework, framework.equals(propFramework));
        }
        objs.put("framework", propFramework);
        objs.put("frameworks", frameworks);

        objs.put("fileContentDataType", additionalProperties.get(FILE_CONTENT_DATA_TYPE));

        return objs;
    }
    
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> models) {     
        
        // Add additional filename information for model imports in the apis
        List<Map<String, Object>> imports = (List<Map<String, Object>>) operations.get("imports");
        for (Map<String, Object> im : imports) {
            im.put("filename", ((String) im.get("import")).replace(".", File.separator));
            im.put("classname", getModelnameFromModelFilename(im.get("import").toString()));
        }
        
        @SuppressWarnings("unchecked")
        Map<String, Object> operationsMap = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operationsMap.get("operation");
        for (CodegenOperation operation: operationList) {
            List<CodegenResponse> responses = operation.responses;
            operation.returnType = this.getReturnType(responses);          
        }
        return operations;
    }
    
    /**
     * Returns the correct return type based on all 2xx HTTP responses defined for an operation.
     * @param responses all CodegenResponses defined for one operation
     * @return TypeScript return type
     */
    private String getReturnType(List<CodegenResponse> responses) {
        Set<String> returnTypes = new HashSet<String>();
        for (CodegenResponse response: responses) {
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
    
    private String getModelnameFromModelFilename(String filename) {
        String name = filename.substring((modelPackage() + File.separator).length());
        return camelize(name);
    }
    
    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        if ("_".equals(name)) {
            name = "_u";
        }

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        name = getNameUsingModelPropertyNaming(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = camelize("model_" + name);
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            String modelName = camelize("model_" + name); // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        if (languageSpecificPrimitives.contains(name)) {
            String modelName = camelize("model_" + name);
            LOGGER.warn(name + " (model name matches existing language type) cannot be used as a model name. Renamed to " + modelName);
            return modelName;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }


    @Override
    protected String getParameterDataType(Parameter parameter, Schema p) {
        // handle enums of various data types
        Schema inner;
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema mp1 = (ArraySchema) p;
            inner = mp1.getItems();
            return this.getSchemaType(p) + "<" + this.getParameterDataType(parameter, inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            inner = (Schema) p.getAdditionalProperties();
            return "{ [key: string]: " + this.getParameterDataType(parameter, inner) + "; }";
        } else if (ModelUtils.isStringSchema(p)) {
            // Handle string enums
            if (p.getEnum() != null) {
                return enumValuesToEnumTypeUnion(p.getEnum(), "string");
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            // Handle integer enums
            if (p.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(p.getEnum()));
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            // Handle double enums
            if (p.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(p.getEnum()));
            }
        }
        return this.getTypeDeclaration(p);
    }

    /**
     * Converts a list of strings to a literal union for representing enum values as a type.
     * Example output: 'available' | 'pending' | 'sold'
     *
     * @param values   list of allowed enum values
     * @param dataType either "string" or "number"
     * @return a literal union for representing enum values as a type
     */
    protected String enumValuesToEnumTypeUnion(List<String> values, String dataType) {
        StringBuilder b = new StringBuilder();
        boolean isFirst = true;
        for (String value : values) {
            if (!isFirst) {
                b.append(" | ");
            }
            b.append(toEnumValue(value.toString(), dataType));
            isFirst = false;
        }
        return b.toString();
    }

    /**
     * Converts a list of numbers to a literal union for representing enum values as a type.
     * Example output: 3 | 9 | 55
     *
     * @param values a list of numbers
     * @return a literal union for representing enum values as a type
     */
    protected String numericEnumValuesToEnumTypeUnion(List<Number> values) {
        List<String> stringValues = new ArrayList<>();
        for (Number value : values) {
            stringValues.add(value.toString());
        }
        return enumValuesToEnumTypeUnion(stringValues, "number");
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            return UNDEFINED_VALUE;
        } else if (ModelUtils.isDateSchema(p)) {
            return UNDEFINED_VALUE;
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return UNDEFINED_VALUE;
        } else if (ModelUtils.isNumberSchema(p)) {
           if (p.getDefault() != null) {
             return p.getDefault().toString();
           }
           return UNDEFINED_VALUE;
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            return UNDEFINED_VALUE;
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "'" + (String) p.getDefault() + "'";
            }
            return UNDEFINED_VALUE;
        } else {
            return UNDEFINED_VALUE;
        }

    }
    
    @Override
    protected boolean isReservedWord(String word) {
        // NOTE: This differs from super's implementation in that TypeScript does _not_ want case insensitive matching.
        return reservedWords.contains(word);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type))
                return type;
        } else
            type = openAPIType;
        return toModelName(type);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        // append _ at the beginning, e.g. _return
        if (isReservedWord(operationId)) {
            return escapeReservedWord(camelize(sanitizeName(operationId), true));
        }

        return camelize(sanitizeName(operationId), true);
    }

    public void setModelPropertyNaming(String naming) {
        if ("original".equals(naming) || "camelCase".equals(naming) ||
                "PascalCase".equals(naming) || "snake_case".equals(naming)) {
            this.modelPropertyNaming = naming;
        } else {
            throw new IllegalArgumentException("Invalid model property naming '" +
                    naming + "'. Must be 'original', 'camelCase', " +
                    "'PascalCase' or 'snake_case'");
        }
    }

    public String getModelPropertyNaming() {
        return this.modelPropertyNaming;
    }

    public String getNameUsingModelPropertyNaming(String name) {
        switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
            case original:
                return name;
            case camelCase:
                return camelize(name, true);
            case PascalCase:
                return camelize(name);
            case snake_case:
                return underscore(name);
            default:
                throw new IllegalArgumentException("Invalid model property naming '" +
                        name + "'. Must be 'original', 'camelCase', " +
                        "'PascalCase' or 'snake_case'");
        }

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
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "Empty";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return camelize(getSymbolName(name));
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

        // camelize the enum variable name
        // ref: https://basarat.gitbooks.io/typescript/content/docs/enums.html
        enumName = camelize(enumName);

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name) + "Enum";

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        List<Map<String, Object>> models = (List<Map<String, Object>>) postProcessModelsEnum(objs).get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
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
        }
        for (Map<String, Object> mo : models) {
            CodegenModel cm = (CodegenModel) mo.get("model");
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
                tsImport.put("filename", toModelFilename(im));
                tsImports.add(tsImport);
            }
        }
        return tsImports;
    }


    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);

        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                if (cm.discriminator != null && cm.children != null) {
                    for (CodegenModel child : cm.children) {
                        this.setDiscriminatorValue(child, cm.discriminator.getPropertyName(), this.getDiscriminatorValue(child));
                    }
                }
            }
        }
        return result;
    }

    private void setDiscriminatorValue(CodegenModel model, String baseName, String value) {
        for (CodegenProperty prop : model.allVars) {
            if (prop.baseName.equals(baseName)) {
                prop.discriminatorValue = value;
            }
        }
        if (model.children != null) {
            final boolean newDiscriminator = model.discriminator != null;
            for (CodegenModel child : model.children) {
                this.setDiscriminatorValue(child, baseName, newDiscriminator ? value : this.getDiscriminatorValue(child));
            }
        }
    }

    private String getDiscriminatorValue(CodegenModel model) {
        return model.vendorExtensions.containsKey(X_DISCRIMINATOR_TYPE) ?
                (String) model.vendorExtensions.get(X_DISCRIMINATOR_TYPE) : model.classname;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ', " to avoid code injection
        return input.replace("\"", "").replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
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

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming((String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }

        convertPropertyToBooleanAndWriteBack(CodegenConstants.SUPPORTS_ES6);

        // change package names
        apiPackage = this.apiPackage + ".apis";
        modelPackage = this.modelPackage + ".models";
        testPackage = this.testPackage + ".tests";

        additionalProperties.putIfAbsent(FRAMEWORK_SWITCH, FRAMEWORKS[0]);
        supportingFiles.add(new SupportingFile("index.mustache", "index.ts"));

        String httpLibName = this.getHttpLibForFramework(additionalProperties.get(FRAMEWORK_SWITCH).toString());
        supportingFiles.add(new SupportingFile(
              "http"  + File.separator + httpLibName + ".mustache",
              "http", httpLibName + ".ts"
        ));

        Object propPlatform = additionalProperties.get(PLATFORM_SWITCH);
        if (propPlatform == null) {
            propPlatform = "browser";
            additionalProperties.put("platform", propPlatform);
        }

        Map<String, Boolean> platforms = new HashMap<>();
        for (String platform: PLATFORMS) {
            platforms.put(platform, platform.equals(propPlatform));
        }
        additionalProperties.put("platforms", platforms);

        additionalProperties.putIfAbsent(FILE_CONTENT_DATA_TYPE, propPlatform.equals("node") ? "Buffer" : "Blob");

        final boolean useRxJS = convertPropertyToBooleanAndWriteBack(USE_RXJS_SWITCH);
        if (!useRxJS) {
            supportingFiles.add(new SupportingFile("rxjsStub.mustache", "rxjsStub.ts"));
        }

        final boolean useInversify = convertPropertyToBooleanAndWriteBack(USE_INVERSIFY_SWITCH);
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
        if (additionalProperties.containsKey(NPM_NAME)) {
            setNpmName(additionalProperties.get(NPM_NAME).toString());
        }

        if (additionalProperties.containsKey(NPM_VERSION)) {
            setNpmVersion(additionalProperties.get(NPM_VERSION).toString());
        }

        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }
    }

    private String getHttpLibForFramework(String object) {
        return this.frameworkToHttpLibMap.get(object);
    }


    @Override
    public String getTypeDeclaration(Schema p) {
        Schema inner;
        if (ModelUtils.isArraySchema(p)) {
            inner = ((ArraySchema) p).getItems();
            return this.getSchemaType(p) + "<" + this.getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            inner = (Schema) p.getAdditionalProperties();
            return "{ [key: string]: " + this.getTypeDeclaration(inner) + "; }";
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
        codegenModel.additionalPropertiesType = getTypeDeclaration((Schema) schema.getAdditionalProperties());
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }
}
