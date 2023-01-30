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

import com.samskivert.mustache.Mustache.Lambda;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

//import com.sun.media.sound.InvalidDataException;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(RClientCodegen.class);

    protected String packageName = "openapi";
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String testFolder = "tests/testthat";
    protected boolean returnExceptionOnFailure = false;
    protected String exceptionPackage = "default";
    protected Map<String, String> exceptionPackages = new LinkedHashMap<String, String>();
    protected Set<String> itemReservedWords = new TreeSet<String>();

    public static final String EXCEPTION_PACKAGE = "exceptionPackage";
    public static final String USE_DEFAULT_EXCEPTION = "useDefaultExceptionHandling";
    public static final String USE_RLANG_EXCEPTION = "useRlangExceptionHandling";
    public static final String GENERATE_WRAPPER = "generateWrapper";
    public static final String DEFAULT = "default";
    public static final String RLANG = "rlang";
    public static final String HTTR = "httr";
    public static final String HTTR2 = "httr2";

    // naming convention for operationId (function names in the API)
    public static final String OPERATIONID_NAMING = "operationIdNaming";

    protected boolean useDefaultExceptionHandling = false;
    protected boolean useRlangExceptionHandling = false;
    protected String errorObjectType;
    protected String operationIdNaming;
    protected boolean generateWrapper;
    protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup

    private Map<String, String> schemaKeyToModelNameCache = new HashMap<>();

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "r";
    }

    public String getHelp() {
        return "Generates a R client library (beta).";
    }

    public RClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        outputFolder = "generated-code/r";
        modelTemplateFiles.put("model.mustache", ".R");
        apiTemplateFiles.put("api.mustache", ".R");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        embeddedTemplateDir = templateDir = "r";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        setReservedWordsLowerCase(
                Arrays.asList(
                        // reserved words: https://stat.ethz.ch/R-manual/R-devel/library/base/html/Reserved.html
                        "if", "else", "repeat", "while", "function", "for", "in",
                        "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN",
                        "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
                        // reserved words in API client
                        "ApiResponse", "data_file"
                )
        );

        // these are reserved words in items: https://github.com/r-lib/R6/blob/main/R/r6_class.R#L484
        itemReservedWords.add("self");
        itemReservedWords.add("private");
        itemReservedWords.add("super");

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("integer");
        languageSpecificPrimitives.add("numeric");
        languageSpecificPrimitives.add("character");
        languageSpecificPrimitives.add("data.frame");
        languageSpecificPrimitives.add("object");

        typeMapping.clear();
        typeMapping.put("integer", "integer");
        typeMapping.put("long", "integer");
        typeMapping.put("number", "numeric");
        typeMapping.put("float", "numeric");
        typeMapping.put("double", "numeric");
        typeMapping.put("decimal", "numeric");
        typeMapping.put("boolean", "character");
        typeMapping.put("string", "character");
        typeMapping.put("UUID", "character");
        typeMapping.put("URI", "character");
        typeMapping.put("date", "character");
        typeMapping.put("DateTime", "character");
        typeMapping.put("password", "character");
        typeMapping.put("file", "data.frame");
        typeMapping.put("binary", "data.frame");
        typeMapping.put("ByteArray", "character");
        typeMapping.put("map", "map");
        typeMapping.put("object", "object");

        // no need for import mapping as R doesn't require api,model import
        // https://github.com/OpenAPITools/openapi-generator/issues/2217
        importMapping.clear();

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "R package name (convention: lowercase).")
                .defaultValue("openapi"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "R package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.EXCEPTION_ON_FAILURE, CodegenConstants.EXCEPTION_ON_FAILURE_DESC)
                .defaultValue(Boolean.FALSE.toString()));

        CliOption operationIdNaming = CliOption.newString(OPERATIONID_NAMING, "Naming convention for operationId (function name in the API)");
        Map<String, String> operationIdNamingOptions = new HashMap<>();
        operationIdNamingOptions.put("snake_case", "Snake case");
        operationIdNamingOptions.put("camelCase", "Camel case");
        operationIdNamingOptions.put("PascalCase", "Pascal case (default)");
        operationIdNaming.setEnum(operationIdNamingOptions);
        cliOptions.add(operationIdNaming);

        exceptionPackages.put(DEFAULT, "Use stop() for raising exceptions.");
        exceptionPackages.put(RLANG, "Use rlang package for exceptions.");

        CliOption exceptionPackage = new CliOption(EXCEPTION_PACKAGE, "Specify the exception handling package");
        exceptionPackage.setEnum(exceptionPackages);
        exceptionPackage.setDefault(DEFAULT);
        cliOptions.add(exceptionPackage);

        cliOptions.add(CliOption.newString(CodegenConstants.ERROR_OBJECT_TYPE, "Error object type."));

        supportedLibraries.put(HTTR2, "httr2 (https://httr2.r-lib.org/)");
        supportedLibraries.put(HTTR, "httr (https://cran.r-project.org/web/packages/httr/index.html)");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "HTTP library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        // set httr as the default
        libraryOption.setDefault(HTTR);
        cliOptions.add(libraryOption);
        setLibrary(HTTR);

        cliOptions.add(CliOption.newBoolean(GENERATE_WRAPPER, "Generate a wrapper class (single point of access) for the R client. This option only works with `httr2` library."));

        // option to change how we process + set the data in the 'additionalProperties' keyword.
        CliOption disallowAdditionalPropertiesIfNotPresentOpt = CliOption.newBoolean(
                CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT,
                CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT_DESC).defaultValue(Boolean.TRUE.toString());
        Map<String, String> disallowAdditionalPropertiesIfNotPresentOpts = new HashMap<>();
        disallowAdditionalPropertiesIfNotPresentOpts.put("false",
                "The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.");
        disallowAdditionalPropertiesIfNotPresentOpts.put("true",
                "Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.");
        disallowAdditionalPropertiesIfNotPresentOpt.setEnum(disallowAdditionalPropertiesIfNotPresentOpts);
        cliOptions.add(disallowAdditionalPropertiesIfNotPresentOpt);
        this.setDisallowAdditionalPropertiesIfNotPresent(true);

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            setPackageVersion("1.0.0");
        }

        if (additionalProperties.containsKey(CodegenConstants.EXCEPTION_ON_FAILURE)) {
            setReturnExceptionOnFailure(Boolean.parseBoolean(
                    additionalProperties.get(CodegenConstants.EXCEPTION_ON_FAILURE).toString()));
        } else {
            setReturnExceptionOnFailure(false);
        }

        if (additionalProperties.containsKey(EXCEPTION_PACKAGE)) {
            setExceptionPackageToUse(additionalProperties.get(EXCEPTION_PACKAGE).toString());
        } else {
            setExceptionPackageToUse(DEFAULT);
        }

        if (additionalProperties.containsKey(CodegenConstants.ERROR_OBJECT_TYPE)) {
            this.setErrorObjectType(additionalProperties.get(CodegenConstants.ERROR_OBJECT_TYPE).toString());
        }
        additionalProperties.put(CodegenConstants.ERROR_OBJECT_TYPE, errorObjectType);

        if (additionalProperties.containsKey(OPERATIONID_NAMING)) {
            this.setOperationIdNaming(additionalProperties.get(OPERATIONID_NAMING).toString());
        } else {
            this.setOperationIdNaming("PascalCase"); // default to PascalCase for backward compatibility
        }
        additionalProperties.put(CodegenConstants.ERROR_OBJECT_TYPE, errorObjectType);

        if (additionalProperties.containsKey(GENERATE_WRAPPER)) {
            this.setGenerateWrapper(Boolean.parseBoolean(
                    additionalProperties.get(GENERATE_WRAPPER).toString()));
        } else {
            this.setGenerateWrapper(false);
        }

        if (additionalProperties.containsKey(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP)) {
            setUseOneOfDiscriminatorLookup(convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP));
        } else {
            additionalProperties.put(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, useOneOfDiscriminatorLookup);
        }

        if (additionalProperties.containsKey(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT)) {
            this.setDisallowAdditionalPropertiesIfNotPresent(Boolean.parseBoolean(additionalProperties
                    .get(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT).toString()));
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        additionalProperties.put(CodegenConstants.EXCEPTION_ON_FAILURE, returnExceptionOnFailure);
        additionalProperties.put(USE_DEFAULT_EXCEPTION, this.useDefaultExceptionHandling);
        additionalProperties.put(USE_RLANG_EXCEPTION, this.useRlangExceptionHandling);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        modelTestTemplateFiles.put("model_test.mustache", ".R");
        apiTestTemplateFiles.put("api_test.mustache", ".R");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("description.mustache", "", "DESCRIPTION"));
        supportingFiles.add(new SupportingFile("Rbuildignore.mustache", "", ".Rbuildignore"));
        supportingFiles.add(new SupportingFile(".travis.yml.mustache", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("ApiResponse.mustache", File.separator + "R", "api_response.R"));
        supportingFiles.add(new SupportingFile("api_client.mustache", File.separator + "R", "api_client.R"));
        supportingFiles.add(new SupportingFile("NAMESPACE.mustache", "", "NAMESPACE"));
        supportingFiles.add(new SupportingFile("testthat.mustache", File.separator + "tests", "testthat.R"));
        supportingFiles.add(new SupportingFile("r-client.mustache", File.separator + ".github" + File.separator + "workflows", "r-client.yaml"));
        supportingFiles.add(new SupportingFile("lintr.mustache", "", ".lintr"));

        if (HTTR.equals(getLibrary())) {
            // for httr
            setLibrary(HTTR);
        } else if (HTTR2.equals(getLibrary())) {
            // for httr2
            setLibrary(HTTR2);
            additionalProperties.put("isHttr2", Boolean.TRUE);
            if (generateWrapper) { // generateWrapper option only supports in httr2 library
                supportingFiles.add(new SupportingFile("api_wrapper.mustache", "R", packageName.toLowerCase(Locale.ROOT) + "_api.R"));
            }
        } else {
            throw new IllegalArgumentException("Invalid HTTP library " + getLibrary() + ". Only httr, httr2 are supported.");
        }

        // add lambda for mustache templates to fix license field
        additionalProperties.put("lambdaLicense", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                String content = fragment.execute();
                content = content.trim().replace("Apache-2.0", "Apache License 2.0");
                writer.write(content);
            }
        });

        // add lambda for mustache templates to escape %
        additionalProperties.put("lambdaRdocEscape", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                String content = fragment.execute();
                content = content.trim().replace("%", "\\%");
                writer.write(content);
            }
        });

    }

    @Override
    public String escapeReservedWord(String name) {
        // Can't start with an underscore, as our fields need to start with an
        // UppercaseLetter so that R treats them as public/visible.

        // Options?
        // - MyName
        // - AName
        // - TheName
        // - XName
        // - X_Name
        // ... or maybe a suffix?
        // - Name_ ... think this will work.
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return camelize(name) + '_';
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + "R" + File.separator;
    }

    public String modelFileFolder() {
        return outputFolder + File.separator + "R" + File.separator;
    }

    @Override
    public String toParamName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        // convert variable name to snake case
        // PetId => pet_id
        name = underscore(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name))
            name = "var_" + name;

        // for reserved word or word starting with number, append _
        if (name.matches("^\\d.*"))
            name = "var_" + name;

        return name;
    }

    @Override
    public String toVarName(String name) {
        // escape item reserved words with "item_" prefix
        if (itemReservedWords.contains(name)) {
            LOGGER.info("The item `{}` has been renamed to `item_{}` as it's a reserved word.", name, name);
            return "item_" + name;
        }

        if ("".equals(name)) {
            LOGGER.warn("Empty item name `` (empty string) has been renamed to `empty_string` to avoid compilation errors.");
            return "empty_string";
        }

        // don't do anything as we'll put property name inside ` `, e.g. `date-time`
        return name;
    }

    @Override
    public String toModelFilename(String name) {
        return underscore(toModelName(name));
    }

    @Override
    public String toModelName(String name) {
        // We need to check if schema-mapping has a different model for this class, so we use it
        // instead of the auto-generated one.
        if (schemaMapping.containsKey(name)) {
            return schemaMapping.get(name);
        }

        // memoization
        String origName = name;
        if (schemaKeyToModelNameCache.containsKey(origName)) {
            return schemaKeyToModelNameCache.get(origName);
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
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

        schemaKeyToModelNameCache.put(origName, camelize(name));
        return camelize(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PetApi.r => pet_api.r
        return underscore(name + "_api");
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
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
    public String toApiName(String name) {
        return camelize(super.toApiName(name));
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "(" + getTypeDeclaration(inner) + ")";
        }

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String openAPIType = getSchemaType(p);
        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }

        if (typeMapping.containsValue(openAPIType)) {
            return openAPIType;
        }

        if (languageSpecificPrimitives.contains(openAPIType)) {
            return openAPIType;
        }

        return toModelName(openAPIType);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type))
                return (type);
        } else {
            type = openAPIType;
        }
        return type;
    }

    @Override
    public String toOperationId(String operationId) {
        String sanitizedOperationId = sanitizeName(operationId);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(sanitizedOperationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, underscore("call_" + operationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        if ("PascalCase".equals(operationIdNaming)) {
            return camelize(sanitizedOperationId);
        } else if ("camelCase".equals(operationIdNaming)) {
            return camelize(sanitizedOperationId, LOWERCASE_FIRST_LETTER);
        } else if ("snake_case".equals(operationIdNaming)) {
            return underscore(sanitizedOperationId);
        } else {
            LOGGER.error("Invalid operationIdNaming: {}. Please report the issue. Default to PascalCase for the time being", operationIdNaming);
            return camelize(sanitizedOperationId);
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            for (CodegenProperty var : cm.vars) {
                // check to see if base name is an empty string
                if ("".equals(var.baseName)) {
                    LOGGER.debug("Empty baseName `` (empty string) in the model `{}` has been renamed to `empty_string` to avoid compilation errors.", cm.classname);
                    var.baseName = "empty_string";
                }

                // create extension x-r-doc-type to store the data type in r doc format
                var.vendorExtensions.put("x-r-doc-type", constructRdocType(var));
            }

            // apply the same fix, enhancement for allVars
            for (CodegenProperty var : cm.allVars) {
                // check to see if base name is an empty string
                if ("".equals(var.baseName)) {
                    LOGGER.debug("Empty baseName `` (empty string) in the model `{}` has been renamed to `empty_string` to avoid compilation errors.", cm.classname);
                    var.baseName = "empty_string";
                }

                // create extension x-r-doc-type to store the data type in r doc format
                var.vendorExtensions.put("x-r-doc-type", constructRdocType(var));
            }
        }
        return postProcessModelsEnum(objs);
    }

    @Override
    protected boolean needToImport(String type) {
        return !languageSpecificPrimitives.contains(type);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setReturnExceptionOnFailure(boolean returnExceptionOnFailure) {
        this.returnExceptionOnFailure = returnExceptionOnFailure;
    }

    public void setExceptionPackageToUse(String exceptionPackage) {
        if (DEFAULT.equals(exceptionPackage))
            this.useDefaultExceptionHandling = true;
        if (RLANG.equals(exceptionPackage)) {
            supportingFiles.add(new SupportingFile("api_exception.mustache", File.separator + "R", "api_exception.R"));
            this.useRlangExceptionHandling = true;
        }
    }

    public void setErrorObjectType(final String errorObjectType) {
        this.errorObjectType = errorObjectType;
    }

    public void setGenerateWrapper(final boolean generateWrapper) {
        this.generateWrapper = generateWrapper;
    }

    public void setUseOneOfDiscriminatorLookup(boolean useOneOfDiscriminatorLookup) {
        this.useOneOfDiscriminatorLookup = useOneOfDiscriminatorLookup;
    }

    public boolean getUseOneOfDiscriminatorLookup() {
        return this.useOneOfDiscriminatorLookup;
    }

    public void setOperationIdNaming(final String operationIdNaming) {
        if (!("PascalCase".equals(operationIdNaming) || "camelCase".equals(operationIdNaming) ||
                "snake_case".equals(operationIdNaming))) {
            throw new IllegalArgumentException("Invalid operationIdNaming: " + operationIdNaming +
                    ". Must be PascalCase, camelCase or snake_case");
        }

        if ("snake_case".equals(operationIdNaming)) {
            additionalProperties.put("WithHttpInfo", "_with_http_info");
        } else {
            additionalProperties.put("WithHttpInfo", "WithHttpInfo");
        }

        this.operationIdNaming = operationIdNaming;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("]]", "] ]");
    }

    public Map<String, String> createMapping(String key, String value) {
        Map<String, String> customImport = new HashMap<String, String>();
        customImport.put(key, value);

        return customImport;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            return value;
        } else {
            return escapeText(value);
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "EMPTY";
        }

        // number
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return getSymbolName(name).toUpperCase(Locale.ROOT);
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
        String enumName = underscore(toModelName(property.name)).toUpperCase(Locale.ROOT);

        // remove [] for array or map of enum
        enumName = enumName.replace("[]", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            p.example = p.defaultValue;
            return;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("character".equals(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if ("integer".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if ("numeric".equals(type)) {
            if (example == null) {
                example = "3.4";
            }
        } else if ("data.frame".equals(type)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "File.new('" + escapeText(example) + "')";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = type + "$new()";
        }

        if (example == null) {
            example = "NULL";
        } else if (Boolean.TRUE.equals(p.isArray)) {
            example = "[" + example + "]";
        } else if (Boolean.TRUE.equals(p.isMap)) {
            example = "{'key' => " + example + "}";
        }

        p.example = example;
    }

    /**
     * Return the example value of the parameter. Overrides the
     * setParameterExampleValue(CodegenParameter, Parameter) method in
     * DefaultCodegen to always call setParameterExampleValue(CodegenParameter)
     * in this class, which adds single quotes around strings from the
     * x-example property.
     *
     * @param codegenParameter Codegen parameter
     * @param parameter        Parameter
     */
    public void setParameterExampleValue(CodegenParameter codegenParameter, Parameter parameter) {
        if (parameter.getExample() != null) {
            codegenParameter.example = parameter.getExample().toString();
        } else if (parameter.getExamples() != null && !parameter.getExamples().isEmpty()) {
            Example example = parameter.getExamples().values().iterator().next();
            if (example.getValue() != null) {
                codegenParameter.example = example.getValue().toString();
            }
        } else {
            Schema schema = parameter.getSchema();
            if (schema != null && schema.getExample() != null) {
                codegenParameter.example = schema.getExample().toString();
            }
        }

        setParameterExampleValue(codegenParameter);
    }

    /**
     * Return the default value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                if (!Boolean.valueOf(p.getDefault().toString()))
                    return "FALSE";
                else
                    return "TRUE";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + ((String.valueOf(p.getDefault()))).replaceAll("\"", "\\\"") + "\"";
            }
        } else if (ModelUtils.isDateTimeSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + ((String.valueOf(p.getDefault()))).replaceAll("\"", "\\\"") + "\"";
            }
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
                if (Pattern.compile("\r\n|\r|\n").matcher((String.valueOf(p.getDefault()))).find())
                    return "'''" + p.getDefault().toString() + "'''";
                else
                    return "\"" + ((String.valueOf(p.getDefault()))).replaceAll("\"", "\\\"") + "\"";
            }
        } else if (ModelUtils.isArraySchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        }

        return null;
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + testFolder;
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + testFolder;
    }

    @Override
    public String toApiTestFilename(String name) {
        return "test_" + toApiFilename(name);
    }

    @Override
    public String toModelTestFilename(String name) {
        return "test_" + toModelFilename(name);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap objectMap = objs.getOperations();

        HashMap<String, CodegenModel> modelMaps = new HashMap<>();
        for (ModelMap modelMap : allModels) {
            CodegenModel m = modelMap.getModel();
            modelMaps.put(m.classname, m);
        }

        List<CodegenOperation> operations = objectMap.getOperation();
        for (CodegenOperation operation : operations) {
            for (CodegenParameter cp : operation.allParams) {
                cp.vendorExtensions.put("x-r-example", constructExampleCode(cp, modelMaps));
            }
        }
        return objs;
    }

    /**
     * Return the R doc type (e.g. list(\link{Media}), character)
     *
     * @param codegenProperty Codegen property
     * @return R doc type
     */
    public String constructRdocType(CodegenProperty codegenProperty) {
        if (codegenProperty.isArray) {
            return "list(" + constructRdocType(codegenProperty.items) + ")";
        } else if (codegenProperty.isMap) {
            return "named list(" + constructRdocType(codegenProperty.items) + ")";
        } else if (languageSpecificPrimitives.contains(codegenProperty.dataType)) {
            // primitive type
            return codegenProperty.dataType;
        } else { // model
            return "\\link{" + codegenProperty.dataType + "}";
        }
    }

    public String constructExampleCode(CodegenParameter codegenParameter, HashMap<String, CodegenModel> modelMaps) {
        if (codegenParameter.isArray) { // array
            return "c(" + constructExampleCode(codegenParameter.items, modelMaps, 0) + ")";
        } else if (codegenParameter.isMap) { // map
            return "c(key = " + constructExampleCode(codegenParameter.items, modelMaps, 0) + ")";
        } else if (languageSpecificPrimitives.contains(codegenParameter.dataType)) { // primitive type
            return codegenParameter.example;
        } else { // model
            // look up the model
            if (modelMaps.containsKey(codegenParameter.dataType)) {
                return constructExampleCode(modelMaps.get(codegenParameter.dataType), modelMaps, 0);
            } else {
                LOGGER.error("Error in constructing examples. Failed to look up the model {}", codegenParameter.dataType);
                return "TODO";
            }
        }
    }

    public String constructExampleCode(CodegenProperty codegenProperty, HashMap<String, CodegenModel> modelMaps, int depth) {
        if (depth > 10) return "...";
        depth++;

        if (codegenProperty.isArray) { // array
            return "c(" + constructExampleCode(codegenProperty.items, modelMaps, depth) + ")";
        } else if (codegenProperty.isMap) { // map
            return "c(key = " + constructExampleCode(codegenProperty.items, modelMaps, depth) + ")";
        } else if (languageSpecificPrimitives.contains(codegenProperty.dataType)) { // primitive type
            if ("character".equals(codegenProperty.dataType)) {
                if (StringUtils.isEmpty(codegenProperty.example)) {
                    return "\"" + codegenProperty.example + "\"";
                } else {
                    if (Boolean.TRUE.equals(codegenProperty.isEnum)) { // enum
                        return "\"" + ((List<Object>) codegenProperty.allowableValues.get("values")).get(0) + "\"";
                    } else {
                        return "\"" + codegenProperty.name + "_example\"";
                    }
                }
            } else { // numeric
                if (StringUtils.isEmpty(codegenProperty.example)) {
                    return codegenProperty.example;
                } else {
                    return "123";
                }
            }
        } else {
            // look up the model
            if (modelMaps.containsKey(codegenProperty.dataType)) {
                return constructExampleCode(modelMaps.get(codegenProperty.dataType), modelMaps, depth);
            } else {
                LOGGER.error("Error in constructing examples. Failed to look up the model {}", codegenProperty.dataType);
                return "TODO";
            }
        }
    }

    public String constructExampleCode(CodegenModel codegenModel, HashMap<String, CodegenModel> modelMaps, int depth) {
        if (depth > 10) return "...";
        depth++;

        String example;
        example = codegenModel.name + "$new(";
        List<String> propertyExamples = new ArrayList<>();
        // required properties first
        for (CodegenProperty codegenProperty : codegenModel.requiredVars) {
            propertyExamples.add(constructExampleCode(codegenProperty, modelMaps, depth));
        }

        // optional properties second
        for (CodegenProperty codegenProperty : codegenModel.optionalVars) {
            propertyExamples.add(constructExampleCode(codegenProperty, modelMaps, depth));
        }

        example += StringUtils.join(propertyExamples, ", ");
        example += ")";
        return example;
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# This generator has been refactored by wing328 (https://github.com/wing328)   #");
        System.out.println("# Please support his work directly by purchasing a copy of the eBook \ud83d\udcd8        #");
        System.out.println("# - OpenAPI Generator for R Developers                http://bit.ly/3lpywTG    #");
        System.out.println("################################################################################");
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.R;
    }

    @Override
    public String toRegularExpression(String pattern) {
        if (pattern == null) {
            return null;
        }

        // remove leading '/'
        if (pattern.charAt(0) == '/') {
            pattern = pattern.substring(1);
        }

        // remove trailing '/'
        if (pattern.charAt(pattern.length() - 1) == '/') {
            pattern = pattern.substring(0, pattern.length() - 1);
        }

        return escapeText(pattern);
    }
}
