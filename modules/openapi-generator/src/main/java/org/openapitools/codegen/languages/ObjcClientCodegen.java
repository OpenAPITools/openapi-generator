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
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class ObjcClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(ObjcClientCodegen.class);

    public static final String CLASS_PREFIX = "classPrefix";
    public static final String POD_NAME = "podName";
    public static final String AUTHOR_NAME = "authorName";
    public static final String AUTHOR_EMAIL = "authorEmail";
    public static final String LICENSE = "license";
    public static final String GIT_REPO_URL = "gitRepoURL";
    public static final String DEFAULT_LICENSE = "Proprietary";
    public static final String CORE_DATA = "coreData";

    protected Set<String> foundationClasses = new HashSet<>();
    protected String podName = "OpenAPIClient";
    protected String podVersion = "1.0.0";
    protected String classPrefix = "OAI";
    protected String authorName = "OpenAPI";
    protected String authorEmail = "team@openapitools.org";
    protected String license = DEFAULT_LICENSE;
    protected String gitRepoURL = "https://github.com/openapitools/openapi-generator";
    protected String[] specialWords = {"new", "copy"};
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String modelFilesPath = "Model/";
    protected String coreFilesPath = "Core/";
    protected String apiFilesPath = "Api/";

    protected boolean generateCoreData = false;

    protected Set<String> advancedMappingTypes = new HashSet<>();

    public ObjcClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
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
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        supportsInheritance = true;
        outputFolder = "generated-code" + File.separator + "objc";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".m");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-body.mustache", ".m");
        embeddedTemplateDir = templateDir = "objc";
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        defaultIncludes.clear();
        defaultIncludes.add("bool");
        defaultIncludes.add("BOOL");
        defaultIncludes.add("int");
        defaultIncludes.add("NSURL");
        defaultIncludes.add("NSString");
        defaultIncludes.add("NSObject");
        defaultIncludes.add("NSArray");
        defaultIncludes.add("NSNumber");
        defaultIncludes.add("NSDate");
        defaultIncludes.add("NSDictionary");
        defaultIncludes.add("NSMutableArray");
        defaultIncludes.add("NSMutableDictionary");
        defaultIncludes.add("NSManagedObject");
        defaultIncludes.add("NSData");

        advancedMappingTypes.add("NSDictionary");
        advancedMappingTypes.add("NSArray");
        advancedMappingTypes.add("NSMutableArray");
        advancedMappingTypes.add("NSMutableDictionary");
        advancedMappingTypes.add("NSObject");
        advancedMappingTypes.add("NSNumber");
        advancedMappingTypes.add("NSURL");
        advancedMappingTypes.add("NSString");
        advancedMappingTypes.add("NSDate");

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("NSNumber");
        languageSpecificPrimitives.add("NSString");
        languageSpecificPrimitives.add("NSObject");
        languageSpecificPrimitives.add("NSDate");
        languageSpecificPrimitives.add("NSData");
        languageSpecificPrimitives.add("NSURL");
        languageSpecificPrimitives.add("bool");
        languageSpecificPrimitives.add("BOOL");

        typeMapping.clear();
        typeMapping.put("enum", "NSString");
        typeMapping.put("date", "NSDate");
        typeMapping.put("datetime", "NSDate");
        typeMapping.put("boolean", "NSNumber");
        typeMapping.put("string", "NSString");
        typeMapping.put("integer", "NSNumber");
        typeMapping.put("int", "NSNumber");
        typeMapping.put("float", "NSNumber");
        typeMapping.put("long", "NSNumber");
        typeMapping.put("double", "NSNumber");
        typeMapping.put("array", "NSArray");
        typeMapping.put("map", "NSDictionary");
        typeMapping.put("number", "NSNumber");
        typeMapping.put("bigdecimal", "NSNumber");
        typeMapping.put("List", "NSArray");
        typeMapping.put("object", "NSObject");
        typeMapping.put("file", "NSURL");
        typeMapping.put("binary", "NSURL");
        typeMapping.put("bytearray", "NSData");
        typeMapping.put("byte", "NSData");
        typeMapping.put("uuid", "NSString");
        typeMapping.put("uri", "NSString");
        typeMapping.put("password", "NSString");

        // ref: http://www.tutorialspoint.com/objective_c/objective_c_basic_syntax.htm
        setReservedWordsLowerCase(
                Arrays.asList(
                        // local variable names in API methods (endpoints)
                        "resourcePath", "pathParams", "queryParams", "headerParams",
                        "responseContentType", "requestContentType", "authSettings",
                        "formParams", "localVarFiles", "bodyParam",
                        // objc reserved words
                        "auto", "else", "long", "switch",
                        "break", "enum", "register", "typedef",
                        "case", "extern", "return", "union",
                        "char", "float", "short", "unsigned",
                        "const", "for", "signed", "void",
                        "continue", "goto", "sizeof", "volatile",
                        "default", "if", "id", "static", "while",
                        "do", "int", "struct", "_Packed",
                        "double", "protocol", "interface", "implementation",
                        "NSObject", "NSInteger", "NSNumber", "CGFloat",
                        "property", "nonatomic", "retain", "strong",
                        "weak", "unsafe_unretained", "readwrite", "readonly",
                        "description", "class"
                ));

        importMapping = new HashMap<>();

        foundationClasses = new HashSet<>(
                Arrays.asList(
                        "NSNumber",
                        "NSObject",
                        "NSString",
                        "NSDate",
                        "NSData",
                        "NSURL",
                        "NSDictionary")
        );

        instantiationTypes.put("array", "NSMutableArray");
        instantiationTypes.put("map", "NSMutableDictionary");

        cliOptions.clear();
        cliOptions.add(new CliOption(CORE_DATA, "Should generate core data models").defaultValue("false"));
        cliOptions.add(new CliOption(CLASS_PREFIX, "prefix for generated classes (convention: Abbreviation of pod name e.g. `HN` for `HackerNews`).`")
                .defaultValue("OAI"));
        cliOptions.add(new CliOption(POD_NAME, "cocoapods package name (convention: CameCase).")
                .defaultValue("OpenAPIClient"));
        cliOptions.add(new CliOption(CodegenConstants.POD_VERSION, "cocoapods package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(AUTHOR_NAME, "Name to use in the podspec file.").defaultValue("OpenAPI"));
        cliOptions.add(new CliOption(AUTHOR_EMAIL, "Email to use in the podspec file.").defaultValue("team@openapitools.org"));
        cliOptions.add(new CliOption(GIT_REPO_URL, "URL for the git repo where this podspec should point to.")
                .defaultValue("https://github.com/openapitools/openapi-generator"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "objc";
    }

    @Override
    public String getHelp() {
        return "Generates an Objective-C client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(POD_NAME)) {
            setPodName((String) additionalProperties.get(POD_NAME));
        }

        if (additionalProperties.containsKey(CodegenConstants.POD_VERSION)) {
            setPodVersion((String) additionalProperties.get(CodegenConstants.POD_VERSION));
        }

        if (additionalProperties.containsKey(CORE_DATA)) {
            Object coreData = additionalProperties.get(CORE_DATA);
            if (((String) coreData).equalsIgnoreCase("true")) {
                generateCoreData = true;
            }
        }
        if (additionalProperties.containsKey(CLASS_PREFIX)) {
            setClassPrefix((String) additionalProperties.get(CLASS_PREFIX));
        }

        if (additionalProperties.containsKey(AUTHOR_NAME)) {
            setAuthorName((String) additionalProperties.get(AUTHOR_NAME));
        }

        if (additionalProperties.containsKey(AUTHOR_EMAIL)) {
            setAuthorEmail((String) additionalProperties.get(AUTHOR_EMAIL));
        }

        if (additionalProperties.containsKey(GIT_REPO_URL)) {
            setGitRepoURL((String) additionalProperties.get(GIT_REPO_URL));
        }

        if (generateCoreData) {
            modelTemplateFiles.put("NSManagedObject-header.mustache", "ManagedObject.h");
            modelTemplateFiles.put("NSManagedObject-body.mustache", "ManagedObject.m");
            modelTemplateFiles.put("NSManagedObjectBuilder-header.mustache", "ManagedObjectBuilder.h");
            modelTemplateFiles.put("NSManagedObjectBuilder-body.mustache", "ManagedObjectBuilder.m");
        }

        additionalProperties.put(POD_NAME, podName);
        additionalProperties.put(CodegenConstants.POD_VERSION, podVersion);
        additionalProperties.put(CLASS_PREFIX, classPrefix);
        additionalProperties.put(AUTHOR_NAME, authorName);
        additionalProperties.put(AUTHOR_EMAIL, authorEmail);
        additionalProperties.put(GIT_REPO_URL, gitRepoURL);
        additionalProperties.put(LICENSE, license);

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
        additionalProperties.put("useCoreData", generateCoreData);

        modelPackage = podName;
        apiPackage = podName;

        supportingFiles.add(new SupportingFile("Object-header.mustache", coreFileFolder(), classPrefix + "Object.h"));
        supportingFiles.add(new SupportingFile("Object-body.mustache", coreFileFolder(), classPrefix + "Object.m"));
        supportingFiles.add(new SupportingFile("QueryParamCollection-header.mustache", coreFileFolder(), classPrefix + "QueryParamCollection.h"));
        supportingFiles.add(new SupportingFile("QueryParamCollection-body.mustache", coreFileFolder(), classPrefix + "QueryParamCollection.m"));
        supportingFiles.add(new SupportingFile("ApiClient-header.mustache", coreFileFolder(), classPrefix + "ApiClient.h"));
        supportingFiles.add(new SupportingFile("ApiClient-body.mustache", coreFileFolder(), classPrefix + "ApiClient.m"));
        supportingFiles.add(new SupportingFile("JSONRequestSerializer-body.mustache", coreFileFolder(), classPrefix + "JSONRequestSerializer.m"));
        supportingFiles.add(new SupportingFile("JSONRequestSerializer-header.mustache", coreFileFolder(), classPrefix + "JSONRequestSerializer.h"));
        supportingFiles.add(new SupportingFile("ResponseDeserializer-body.mustache", coreFileFolder(), classPrefix + "ResponseDeserializer.m"));
        supportingFiles.add(new SupportingFile("ResponseDeserializer-header.mustache", coreFileFolder(), classPrefix + "ResponseDeserializer.h"));
        supportingFiles.add(new SupportingFile("Sanitizer-body.mustache", coreFileFolder(), classPrefix + "Sanitizer.m"));
        supportingFiles.add(new SupportingFile("Sanitizer-header.mustache", coreFileFolder(), classPrefix + "Sanitizer.h"));
        supportingFiles.add(new SupportingFile("Logger-body.mustache", coreFileFolder(), classPrefix + "Logger.m"));
        supportingFiles.add(new SupportingFile("Logger-header.mustache", coreFileFolder(), classPrefix + "Logger.h"));
        supportingFiles.add(new SupportingFile("JSONValueTransformer+ISO8601-body.mustache", coreFileFolder(), "JSONValueTransformer+ISO8601.m"));
        supportingFiles.add(new SupportingFile("JSONValueTransformer+ISO8601-header.mustache", coreFileFolder(), "JSONValueTransformer+ISO8601.h"));
        supportingFiles.add(new SupportingFile("Configuration-protocol.mustache", coreFileFolder(), classPrefix + "Configuration.h"));
        supportingFiles.add(new SupportingFile("DefaultConfiguration-body.mustache", coreFileFolder(), classPrefix + "DefaultConfiguration.m"));
        supportingFiles.add(new SupportingFile("DefaultConfiguration-header.mustache", coreFileFolder(), classPrefix + "DefaultConfiguration.h"));
        supportingFiles.add(new SupportingFile("BasicAuthTokenProvider-header.mustache", coreFileFolder(), classPrefix + "BasicAuthTokenProvider.h"));
        supportingFiles.add(new SupportingFile("BasicAuthTokenProvider-body.mustache", coreFileFolder(), classPrefix + "BasicAuthTokenProvider.m"));
        supportingFiles.add(new SupportingFile("api-protocol.mustache", coreFileFolder(), classPrefix + "Api.h"));
        supportingFiles.add(new SupportingFile("podspec.mustache", "", podName + ".podspec"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        if (generateCoreData) {
            supportingFiles.add(new SupportingFile("xccurrentversion.mustache", (modelPackage() + "/" + modelFilesPath + "/").replace("/", File.separator) + classPrefix + "Model.xcdatamodeld", ".xccurrentversion"));
            supportingFiles.add(new SupportingFile("Model.xcdatamodel.mustache", (modelPackage() + "/" + modelFilesPath + "/").replace("/", File.separator) + classPrefix + "Model.xcdatamodeld" + File.separator + classPrefix + "Model.xcdatamodel", "contents"));
        }
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            return instantiationTypes.get("map");
        } else if (ModelUtils.isArraySchema(p)) {
            return instantiationTypes.get("array");
        } else {
            return null;
        }
    }

    @Override
    public String getTypeDeclaration(String name) {
        if (languageSpecificPrimitives.contains(name) && !foundationClasses.contains(name)) {
            return name;
        } else {
            return name + "*";
        }
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;

        if (openAPIType == null) {
            openAPIType = ""; // set OpenAPI type to empty string if null
        }

        // TODO avoid using toLowerCase as typeMapping should be case-sensitive
        if (typeMapping.containsKey(openAPIType.toLowerCase(Locale.ROOT))) {
            type = typeMapping.get(openAPIType.toLowerCase(Locale.ROOT));
            if (languageSpecificPrimitives.contains(type) && !foundationClasses.contains(type)) {
                return toModelNameWithoutReservedWordCheck(type);
            }
        } else {
            type = openAPIType;
        }
        return toModelNameWithoutReservedWordCheck(type);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            String innerTypeDeclaration = getTypeDeclaration(inner);
            if (innerTypeDeclaration.endsWith("*")) {
                innerTypeDeclaration = innerTypeDeclaration.substring(0, innerTypeDeclaration.length() - 1);
            }
            // In this condition, type of Schema p is array of primitive,
            // return container type with pointer, e.g. `NSArray*<NSString*>*'
            if (languageSpecificPrimitives.contains(innerTypeDeclaration)) {
                return getSchemaType(p) + "<" + innerTypeDeclaration + "*>*";
            }
            // In this condition, type of Schema p is array of model,
            // return container type combine inner type with pointer, e.g. `NSArray<SWGTag>*'
            else {
                for (String sd : advancedMappingTypes) {
                    if (innerTypeDeclaration.startsWith(sd)) {
                        return getSchemaType(p) + "<" + innerTypeDeclaration + "*>*";
                    }
                }
                return getSchemaType(p) + "<" + innerTypeDeclaration + ">*";
            }
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);

            String innerTypeDeclaration = getTypeDeclaration(inner);

            if (innerTypeDeclaration.endsWith("*")) {
                innerTypeDeclaration = innerTypeDeclaration.substring(0, innerTypeDeclaration.length() - 1);
            }
            if (languageSpecificPrimitives.contains(innerTypeDeclaration)) {
                return getSchemaType(p) + "<NSString*, " + innerTypeDeclaration + "*>*";
            } else {
                for (String s : advancedMappingTypes) {
                    if (innerTypeDeclaration.startsWith(s)) {
                        return getSchemaType(p) + "<NSString*, " + innerTypeDeclaration + "*>*";
                    }
                }
                return getSchemaType(p) + "<" + innerTypeDeclaration + ">*";
            }
        } else {
            String openAPIType = getSchemaType(p);
            // In this condition, type of p is objective-c primitive type, e.g. `NSSNumber',
            // return type of p with pointer, e.g. `NSNumber*'
            if (languageSpecificPrimitives.contains(openAPIType) &&
                    foundationClasses.contains(openAPIType)) {
                return openAPIType + "*";
            }
            // In this condition, type of p is c primitive type, e.g. `bool',
            // return type of p, e.g. `bool'
            else if (languageSpecificPrimitives.contains(openAPIType)) {
                return openAPIType;
            }
            // In this condition, type of p is objective-c object type, e.g. `SWGPet',
            // return type of p with pointer, e.g. `SWGPet*'
            else {
                return openAPIType + "*";
            }
        }
    }

    @Override
    public boolean isDataTypeBinary(String dataType) {
        return dataType.toLowerCase(Locale.ROOT).startsWith("nsdata");
    }

    @Override
    public String toModelName(String type) {
        // model name cannot use reserved keyword
        if (reservedWords.contains(type)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {} before further processing",
                    type, "model_" + type);
            type = "model_" + type; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        /* no need for the fix below as objc model starts with prefix (e.g. SWG)
        if (type.matches("^\\d.*")) {
            LOGGER.warn(type + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + type));
            type = "model_" + type; // e.g. 200Response => Model200Response (after camelize)
        }
        */

        return toModelNameWithoutReservedWordCheck(type);
    }

    /*
     * Convert input to proper model name according to ObjC style guide
     * without checking for reserved words
     *
     * @param type Model name
     * @return model Name in ObjC style guide
     */
    public String toModelNameWithoutReservedWordCheck(String type) {
        type = type.replaceAll("[^0-9a-zA-Z_]", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // language build-in classes
        if (typeMapping.keySet().contains(type) ||
                foundationClasses.contains(type) ||
                importMapping.values().contains(type) ||
                defaultIncludes.contains(type) ||
                languageSpecificPrimitives.contains(type)) {
            return camelize(type);
        }
        // custom classes
        else {
            if (!StringUtils.isEmpty(modelNameSuffix)) { // set model suffix
                type = type + "_" + modelNameSuffix;
            }

            if (!StringUtils.isEmpty(modelNamePrefix)) { // set model prefix
                type = modelNamePrefix + "_" + type;
            }

            return classPrefix + camelize(type); // add class prefix
        }
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    protected void setNonArrayMapProperty(CodegenProperty schema, String type) {
        super.setNonArrayMapProperty(schema, type);
        if ("NSDictionary".equals(type)) {
            schema.setter = "initWithDictionary";
        } else {
            schema.setter = "initWithValues";
        }
    }

    @Override
    public String toModelImport(String name) {
        return name;
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace("/", File.separator);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace("/", File.separator);
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
    public String apiFileFolder() {
        return (outputFolder + "/" + apiPackage() + "/" + apiFilesPath).replace("/", File.separator);
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + "/" + modelPackage() + "/" + modelFilesPath).replace("/", File.separator);
    }

    public String coreFileFolder() {
        return (apiPackage() + "/" + coreFilesPath).replace("/", File.separator);
    }

    @Override
    public String toApiName(String name) {
        return classPrefix + camelize(name) + "Api";
    }

    @Override
    public String toApiFilename(String name) {
        return classPrefix + camelize(name) + "Api";
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all upper case, do noting
        if (name.matches("^[A-Z_]$")) {
            return name;
        }

        // if name starting with special word, escape with '_'
        for (String specialWord : specialWords) {
            if (name.matches("(?i:^" + specialWord + ".*)"))
                name = escapeSpecialWord(name);
        }

        if (name.startsWith(classPrefix)) {
            name = name.replaceFirst(classPrefix, ""); //remove the class prefix, e.g. SWGPet => Pet
        }

        // camelize (lower first character) the variable name
        // e.g. `pet_id` to `petId`
        name = camelize(name, true);

        // for reserved word or word starting with number, prepend `_`
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }


        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @SuppressWarnings("static-method")
    public String escapeSpecialWord(String name) {
        return "var_" + name;
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

        return camelize(sanitizeName(operationId), true);
    }

    public void setClassPrefix(String classPrefix) {
        this.classPrefix = classPrefix;
    }

    public void setPodName(String podName) {
        this.podName = podName;
    }

    public void setPodVersion(String podVersion) {
        this.podVersion = podVersion;
    }

    public void setAuthorEmail(String authorEmail) {
        this.authorEmail = authorEmail;
    }

    public void setAuthorName(String authorName) {
        this.authorName = authorName;
    }

    public void setGitRepoURL(String gitRepoURL) {
        this.gitRepoURL = gitRepoURL;
    }

    public void setLicense(String license) {
        this.license = license;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");

        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (!operation.allParams.isEmpty()) {
                    String firstParamName = operation.allParams.get(0).paramName;
                    operation.vendorExtensions.put("x-first-param-alt-name", camelize(firstParamName));
                }
            }
        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty schema) {
        super.postProcessModelProperty(model, schema);
        schema.vendorExtensions.put("x-uppercase-name", camelize(schema.name));
    }

    /**
     * Return the default value of the schema
     * @param p OpenAPI schema object
     * @return string presentation of the default value of the schema
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isDateSchema(p)) {
            // TODO
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return "@" + p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return "@" + p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "@\"" + (String) p.getDefault() + "\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                if (p.getDefault().toString().equalsIgnoreCase("false"))
                    return "@(NO)";
                else
                    return "@(YES)";
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
            p.example = p.defaultValue;
            return;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("NSString*".equalsIgnoreCase(type) || "NSString".equalsIgnoreCase(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "@\"" + escapeText(example) + "\"";
        } else if ("NSNumber*".equalsIgnoreCase(type) || "NSNumber".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "56";
            }
            example = "@" + example;
        /* OBJC uses NSNumber to represent both int, long, double and float
        } else if ("Float".equalsIgnoreCase(type) || "Double".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "3.4";
            } */
        } else if ("BOOLEAN".equalsIgnoreCase(type) || "bool".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "True";
            }
        } else if ("NSURL*".equalsIgnoreCase(type) || "NSURL".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "/path/to/file";
            }
            //[NSURL fileURLWithPath:@"path/to/file"]
            example = "[NSURL fileURLWithPath:@\"" + escapeText(example) + "\"]";
        /*} else if ("NSDate".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "'" + escapeText(example) + "'";*/
        } else if ("NSDate*".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "@\"" + escapeText(example) + "\"";
        } else if ("NSData".equalsIgnoreCase(type)) {
            example = "1234";
        } else if (type != null && !languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            type = type.replace("*", "");
            // e.g. [[SWGPet alloc] init
            example = "[[" + type + " alloc] init]";
        } else {
            LOGGER.warn("Example value for {} not handled properly in setParameterExampleValue", type);
        }

        if (example == null) {
            example = "NULL";
        } else if (Boolean.TRUE.equals(p.isArray)) {
            example = "@[" + example + "]";
        } else if (Boolean.TRUE.equals(p.isMap)) {
            example = "@{@\"key\" : " + example + "}";
        }

        p.example = example;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

}
