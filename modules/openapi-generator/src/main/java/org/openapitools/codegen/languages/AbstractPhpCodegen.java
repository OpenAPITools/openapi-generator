/* Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public abstract class AbstractPhpCodegen extends DefaultCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractPhpCodegen.class);

    public static final String VARIABLE_NAMING_CONVENTION = "variableNamingConvention";
    public static final String PACKAGE_NAME = "packageName";
    public static final String SRC_BASE_PATH = "srcBasePath";
    protected String invokerPackage = "php";
    protected String packageName = "php-base";
    protected String artifactVersion = null;
    protected String srcBasePath = "lib";
    protected String testBasePath = "test";
    protected String docsBasePath = "docs";
    protected String apiDirName = "Api";
    protected String modelDirName = "Model";
    protected String variableNamingConvention = "snake_case";
    protected String apiDocPath = docsBasePath + "/" + apiDirName;
    protected String modelDocPath = docsBasePath + "/" + modelDirName;
    protected String interfaceNamePrefix = "", interfaceNameSuffix = "Interface";
    protected String abstractNamePrefix = "Abstract", abstractNameSuffix = "";
    protected String traitNamePrefix = "", traitNameSuffix = "Trait";

    public AbstractPhpCodegen() {
        super();

        modelTemplateFiles.put("model.mustache", ".php");
        apiTemplateFiles.put("api.mustache", ".php");
        apiTestTemplateFiles.put("api_test.mustache", ".php");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;

        setReservedWordsLowerCase(
                Arrays.asList(
                        // local variables used in api methods (endpoints)
                        "resourcePath", "httpBody", "queryParams", "headerParams",
                        "formParams", "_header_accept", "_tempBody",

                        // PHP reserved words
                        "__halt_compiler", "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class", "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else", "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile", "eval", "exit", "extends", "final", "for", "foreach", "function", "global", "goto", "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset", "list", "namespace", "new", "or", "print", "private", "protected", "public", "require", "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use", "var", "while", "xor")
        );

        // ref: http://php.net/manual/en/language.types.intro.php
        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "bool",
                        "boolean",
                        "int",
                        "integer",
                        "double",
                        "float",
                        "string",
                        "object",
                        "array",
                        "\\DateTime",
                        "\\SplFileObject",
                        "mixed",
                        "number",
                        "void",
                        "byte")
        );

        instantiationTypes.put("array", "array");
        instantiationTypes.put("map", "array");


        // provide primitives to mustache template
        String primitives = "'" + StringUtils.join(languageSpecificPrimitives, "', '") + "'";
        additionalProperties.put("primitives", primitives);

        // ref: https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#data-types
        typeMapping = new HashMap<>();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");
        typeMapping.put("number", "float");
        typeMapping.put("float", "float");
        typeMapping.put("decimal", "float");
        typeMapping.put("double", "double");
        typeMapping.put("string", "string");
        typeMapping.put("byte", "int");
        typeMapping.put("boolean", "bool");
        typeMapping.put("date", "\\DateTime");
        typeMapping.put("Date", "\\DateTime");
        typeMapping.put("DateTime", "\\DateTime");
        typeMapping.put("file", "\\SplFileObject");
        typeMapping.put("map", "array");
        typeMapping.put("array", "array");
        typeMapping.put("list", "array");
        typeMapping.put("object", "object");
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("AnyType", "mixed");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(VARIABLE_NAMING_CONVENTION, "naming convention of variable name, e.g. camelCase.")
                .defaultValue("snake_case"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, "The main namespace to use for all classes. e.g. Yay\\Pets"));
        cliOptions.add(new CliOption(PACKAGE_NAME, "The main package name for classes. e.g. GeneratedPetstore"));
        cliOptions.add(new CliOption(SRC_BASE_PATH, "The directory to serve as source root."));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, "The version to use in the composer package version field. e.g. 1.2.3"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("PHP_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable PHP_POST_PROCESS_FILE not defined so the PHP code may not be properly formatted. To define it, try 'export PHP_POST_PROCESS_FILE=\"/usr/local/bin/prettier --write\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(PACKAGE_NAME)) {
            this.setPackageName((String) additionalProperties.get(PACKAGE_NAME));
        } else {
            additionalProperties.put(PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(SRC_BASE_PATH)) {
            this.setSrcBasePath((String) additionalProperties.get(SRC_BASE_PATH));
        } else {
            additionalProperties.put(SRC_BASE_PATH, srcBasePath);
        }

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));

            // Update the invokerPackage for the default apiPackage and modelPackage
            apiPackage = invokerPackage + "\\" + apiDirName;
            modelPackage = invokerPackage + "\\" + modelDirName;
        } else {
            additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            // Update model package to contain the specified model package name and the invoker package
            modelPackage = invokerPackage + "\\" + (String) additionalProperties.get(CodegenConstants.MODEL_PACKAGE);
        }
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);

        if (additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            // Update api package to contain the specified api package name and the invoker package
            apiPackage = invokerPackage + "\\" + (String) additionalProperties.get(CodegenConstants.API_PACKAGE);
        }
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_VERSION)) {
            this.setArtifactVersion((String) additionalProperties.get(CodegenConstants.ARTIFACT_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }

        if (additionalProperties.containsKey(VARIABLE_NAMING_CONVENTION)) {
            this.setParameterNamingConvention((String) additionalProperties.get(VARIABLE_NAMING_CONVENTION));
        }

        if (additionalProperties.containsKey(CodegenConstants.GIT_USER_ID)) {
            this.setGitUserId((String) additionalProperties.get(CodegenConstants.GIT_USER_ID));
        }

        if (additionalProperties.containsKey(CodegenConstants.GIT_REPO_ID)) {
            this.setGitRepoId((String) additionalProperties.get(CodegenConstants.GIT_REPO_ID));
        }

        if (!this.getComposerPackageName().isEmpty()) {
            additionalProperties.put("composerPackageName", this.getComposerPackageName());
        }

        additionalProperties.put("escapedInvokerPackage", invokerPackage.replace("\\", "\\\\"));

        // make api and model src path available in mustache template
        additionalProperties.put("apiSrcPath", "./" + toSrcPath(apiPackage, srcBasePath));
        additionalProperties.put("modelSrcPath", "./" + toSrcPath(modelPackage, srcBasePath));
        additionalProperties.put("apiTestPath", "./" + testBasePath + "/" + apiDirName);
        additionalProperties.put("modelTestPath", "./" + testBasePath + "/" + modelDirName);

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        // make test path available in mustache template
        additionalProperties.put("testBasePath", testBasePath);

        // make class prefixes and suffixes available in mustache templates
        additionalProperties.put("interfaceNamePrefix", interfaceNamePrefix);
        additionalProperties.put("interfaceNameSuffix", interfaceNameSuffix);
        additionalProperties.put("abstractNamePrefix", abstractNamePrefix);
        additionalProperties.put("abstractNameSuffix", abstractNameSuffix);
        additionalProperties.put("traitNamePrefix", traitNamePrefix);
        additionalProperties.put("traitNameSuffix", traitNameSuffix);

        // apache v2 license
        // supportingFiles.add(new SupportingFile("LICENSE", "", "LICENSE"));

        // all PHP codegens requires Composer, it means that we need to exclude from SVN at least vendor folder
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
    }

    public String getPackageName() {
        return packageName;
    }

    public String toSrcPath(String packageName, String basePath) {
        packageName = packageName.replace(invokerPackage, ""); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        if (basePath != null && basePath.length() > 0) {
            basePath = basePath.replaceAll("[\\\\/]?$", "") + File.separator; // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        }

        // Trim prefix file separators from package path
        String packagePath = StringUtils.removeStart(
            // Replace period, backslash, forward slash with file separator in package name
            packageName.replaceAll("[\\.\\\\/]", Matcher.quoteReplacement("/")),
            File.separator
        );

        // Trim trailing file separators from the overall path
        return StringUtils.removeEnd(basePath + packagePath, File.separator);
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + File.separator + toSrcPath(apiPackage, srcBasePath));
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + File.separator + toSrcPath(modelPackage, srcBasePath));
    }

    @Override
    public String apiTestFileFolder() {
        return (outputFolder + File.separator + testBasePath + File.separator + apiDirName);
    }

    @Override
    public String modelTestFileFolder() {
        return (outputFolder + File.separator + testBasePath + File.separator + modelDirName);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + File.separator + apiDocPath);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + File.separator + modelDocPath);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            if (inner == null) {
                LOGGER.warn("{}(array property) does not have a proper inner type defined.Default to string",
                        ap.getName());
                inner = new StringSchema().description("TODO default missing array inner type to string");
            }
            return getTypeDeclaration(inner) + "[]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            if (inner == null) {
                LOGGER.warn("{}(map property) does not have a proper inner type defined. Default to string", p.getName());
                inner = new StringSchema().description("TODO default missing map inner type to string");
            }
            return getSchemaType(p) + "<string," + getTypeDeclaration(inner) + ">";
        } else if (StringUtils.isNotBlank(p.get$ref())) { // model
            String type = super.getTypeDeclaration(p);
            return (!languageSpecificPrimitives.contains(type))
                    ? "\\" + modelPackage + "\\" + type : type;
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getTypeDeclaration(String name) {
        if (!languageSpecificPrimitives.contains(name)) {
            return "\\" + modelPackage + "\\" + name;
        }
        return super.getTypeDeclaration(name);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;

        if (openAPIType == null) {
            LOGGER.error("OpenAPI Type for {} is null. Default to UNKNOWN_OPENAPI_TYPE instead.", p.getName());
            openAPIType = "UNKNOWN_OPENAPI_TYPE";
        }

        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            } else if (instantiationTypes.containsKey(type)) {
                return type;
            }
            /*
            // comment out the following as php-dt, php-mezzio still need to treat DateTime, SplFileObject as objects
            } else {
                throw new RuntimeException("OpenAPI type `" + openAPIType + "` defined but can't mapped to language type." +
                        " Please report the issue via OpenAPI Generator github repo." +
                        " (if you're not using custom format with proper type mappings provided to openapi-generator)");
            }
            */
        } else {
            type = openAPIType;
        }

        return toModelName(type);
    }

    public String getInvokerPackage() {
        return invokerPackage;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setSrcBasePath(String srcBasePath) {
        this.srcBasePath = srcBasePath;
    }

    public void setParameterNamingConvention(String variableNamingConvention) {
        this.variableNamingConvention = variableNamingConvention;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        if ("camelCase".equals(variableNamingConvention)) {
            // return the name in camelCase style
            // phone_number => phoneNumber
            name = camelize(name, true);
        } else { // default to snake case
            // return the name in underscore style
            // PhoneNumber => phone_number
            name = underscore(name);
        }

        // parameter name starting with number won't compile
        // need to escape it by appending _ at the beginning
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    private String toGenericName(String name) {
        // remove [
        name = name.replaceAll("\\]", "");

        // Note: backslash ("\\") is allowed for e.g. "\\DateTime"
        name = name.replaceAll("[^\\w\\\\]+", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // remove dollar sign
        name = name.replaceAll("$", "");

        // model name cannot use reserved keyword
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    camelize("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        return name;
    }

    @Override
    public String toModelName(String name) {
        name = toGenericName(name);

        // add prefix and/or suffix only if name does not start wth \ (e.g. \DateTime)
        if (!name.matches("^\\\\.*")) {
            if (!StringUtils.isEmpty(modelNamePrefix)) {
                name = modelNamePrefix + "_" + name;
            }

            if (!StringUtils.isEmpty(modelNameSuffix)) {
                name = name + "_" + modelNameSuffix;
            }
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
    public String toModelTestFilename(String name) {
        // should be the same as the model name
        return toModelName(name) + "Test";
    }

    /**
     * Output the proper interface name (capitalized).
     *
     * @param name the name of the interface
     * @return capitalized model name
     */
    public String toInterfaceName(final String name) {
        return camelize(interfaceNamePrefix + name + interfaceNameSuffix);
    }

    /**
     * Output the proper abstract class name (capitalized).
     *
     * @param name the name of the class
     * @return capitalized abstract class name
     */
    public String toAbstractName(final String name) {
        return camelize(abstractNamePrefix + name + abstractNameSuffix);
    }

    /**
     * Output the proper trait name (capitalized).
     *
     * @param name the name of the trait
     * @return capitalized trait name
     */
    public String toTraitName(final String name) {
        return camelize(traitNamePrefix + name + traitNameSuffix);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, camelize(sanitizeName("call_" + operationId), true));
            operationId = "call_" + operationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, camelize(sanitizeName("call_" + operationId), true));
            operationId = "call_" + operationId;
        }

        return camelize(sanitizeName(operationId), true);
    }

    /**
     * Return the default value of the property
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
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
                return "'" + p.getDefault() + "'";
            }
        }

        return null;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            example = p.defaultValue;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("String".equalsIgnoreCase(type) || p.isString) {
            if (example == null) {
                example = "'" + p.paramName + "_example'";
            }
            example = escapeText(example);
        } else if ("Integer".equals(type) || "int".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if ("Float".equalsIgnoreCase(type) || "Double".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "3.4";
            }
        } else if ("BOOLEAN".equalsIgnoreCase(type) || "bool".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "True";
            }
        } else if ("\\SplFileObject".equalsIgnoreCase(type) || p.isFile) {
            if (example == null) {
                example = "/path/to/file.txt";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if ("\\Date".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "new \\DateTime(\"" + escapeText(example) + "\")";
        } else if ("\\DateTime".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "new \\DateTime(\"" + escapeText(example) + "\")";
        } else if ("object".equals(type)) {
            example = "new \\stdClass";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = "new " + getTypeDeclaration(type) + "()";
        } else {
            LOGGER.warn("Type {} not handled properly in setParameterExampleValue", type);
        }

        if (example == null) {
            example = "NULL";
        } else if (Boolean.TRUE.equals(p.isArray)) {
            example = "array(" + example + ")";
        } else if (Boolean.TRUE.equals(p.isMap)) {
            example = "array('key' => " + example + ")";
        }

        p.example = example;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            return value;
        } else {
            return "\'" + escapeText(value) + "\'";
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return "self::" + datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "EMPTY";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return (getSymbolName(name)).toUpperCase(Locale.ROOT);
        }

        // number
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String enumName = sanitizeName(underscore(name).toUpperCase(Locale.ROOT));
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (isReservedWord(enumName) || enumName.matches("\\d.*")) { // reserved word or starts with number
            return escapeReservedWord(enumName);
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = underscore(toGenericName(property.name)).toUpperCase(Locale.ROOT);

        // remove [] for array or map of enum
        enumName = enumName.replace("[]", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            // for API test method name
            // e.g. public function test{{vendorExtensions.x-testOperationId}}()
            op.vendorExtensions.put("x-test-operation-id", camelize(op.operationId));
        }
        return objs;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public String escapeText(String input) {
        if (input == null) {
            return input;
        }

        // Trim the string to avoid leading and trailing spaces.
        return super.escapeText(input).trim();
    }

    public void escapeMediaType(List<CodegenOperation> operationList) {
        for (CodegenOperation op : operationList) {
            if (!op.hasProduces) {
                continue;
            }

            List<Map<String, String>> c = op.produces;
            for (Map<String, String> mediaType : c) {
                // "*/*" causes a syntax error
                if ("*/*".equals(mediaType.get("mediaType"))) {
                    mediaType.put("mediaType", "*_/_*");
                }
            }
        }
    }

    protected String extractSimpleName(String phpClassName) {
        if (phpClassName == null) {
            return null;
        }

        final int lastBackslashIndex = phpClassName.lastIndexOf('\\');
        return phpClassName.substring(lastBackslashIndex + 1);
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }
        String phpPostProcessFile = System.getenv("PHP_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(phpPostProcessFile)) {
            return; // skip if PHP_POST_PROCESS_FILE env variable is not defined
        }
        // only process files with php extension
        if ("php".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = phpPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                p.waitFor();
                int exitValue = p.exitValue();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }

    /**
     * Get Composer package name based on GIT_USER_ID and GIT_REPO_ID.
     *
     * @return package name or empty string on fail
     */
    public String getComposerPackageName() {
        String packageName = this.getGitUserId() + "/" + this.getGitRepoId();
        if (
            packageName.contentEquals("/")
            || packageName.contentEquals("null/null")
            || !Pattern.matches("^[a-z0-9]([_.-]?[a-z0-9]+)*/[a-z0-9](([_.]?|-{0,2})[a-z0-9]+)*$", packageName)
        ) {
            return "";
        }

        return packageName;
    }

    @Override
    public boolean isDataTypeString(String dataType) {
        return "string".equals(dataType);
    }
}
