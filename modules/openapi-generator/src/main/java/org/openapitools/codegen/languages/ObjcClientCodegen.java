package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.ArrayModel;
import io.swagger.models.Model;
import io.swagger.models.properties.*;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

public class ObjcClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String CLASS_PREFIX = "classPrefix";
    public static final String POD_NAME = "podName";
    public static final String AUTHOR_NAME = "authorName";
    public static final String AUTHOR_EMAIL = "authorEmail";
    public static final String LICENSE = "license";
    public static final String GIT_REPO_URL = "gitRepoURL";
    public static final String DEFAULT_LICENSE = "Proprietary";
    public static final String CORE_DATA = "coreData";

    protected Set<String> foundationClasses = new HashSet<String>();
    protected String podName = "SwaggerClient";
    protected String podVersion = "1.0.0";
    protected String classPrefix = "SWG";
    protected String authorName = "Swagger";
    protected String authorEmail = "apiteam@swagger.io";
    protected String license = DEFAULT_LICENSE;
    protected String gitRepoURL = "https://github.com/swagger-api/swagger-codegen";
    protected String[] specialWords = {"new", "copy"};
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String modelFilesPath = "Model/";
    protected String coreFilesPath = "Core/";
    protected String apiFilesPath = "Api/";

    protected boolean generateCoreData = false;

    protected Set<String> advancedMapingTypes = new HashSet<String>();

    public ObjcClientCodegen() {
        super();
        supportsInheritance = true;
        outputFolder = "generated-code" + File.separator + "objc";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".m");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-body.mustache", ".m");
        embeddedTemplateDir = templateDir = "objc";
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

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

        advancedMapingTypes.add("NSDictionary");
        advancedMapingTypes.add("NSArray");
        advancedMapingTypes.add("NSMutableArray");
        advancedMapingTypes.add("NSMutableDictionary");
        advancedMapingTypes.add("NSObject");
        advancedMapingTypes.add("NSNumber");
        advancedMapingTypes.add("NSURL");
        advancedMapingTypes.add("NSString");
        advancedMapingTypes.add("NSDate");

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
        typeMapping.put("binary", "NSData");
        typeMapping.put("bytearray", "NSData");
        typeMapping.put("byte", "NSData");
        typeMapping.put("uuid", "NSString");
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
                    "description"
                ));

        importMapping = new HashMap<String, String>();

        foundationClasses = new HashSet<String>(
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
                .defaultValue("SWG"));
        cliOptions.add(new CliOption(POD_NAME, "cocoapods package name (convention: CameCase).")
                .defaultValue("SwaggerClient"));
        cliOptions.add(new CliOption(CodegenConstants.POD_VERSION, "cocoapods package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(AUTHOR_NAME, "Name to use in the podspec file.").defaultValue("Swagger"));
        cliOptions.add(new CliOption(AUTHOR_EMAIL, "Email to use in the podspec file.").defaultValue("apiteam@swagger.io"));
        cliOptions.add(new CliOption(GIT_REPO_URL, "URL for the git repo where this podspec should point to.")
                .defaultValue("https://github.com/swagger-api/swagger-codegen"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "hides the timestamp when files were generated")
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

        // default HIDE_GENERATION_TIMESTAMP to true
        if (!additionalProperties.containsKey(CodegenConstants.HIDE_GENERATION_TIMESTAMP)) {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, Boolean.TRUE.toString());
        } else {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                    Boolean.valueOf(additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP).toString()));
        }

        if (additionalProperties.containsKey(POD_NAME)) {
            setPodName((String) additionalProperties.get(POD_NAME));
        }

        if (additionalProperties.containsKey(CodegenConstants.POD_VERSION)) {
            setPodVersion((String) additionalProperties.get(CodegenConstants.POD_VERSION));
        }

        if (additionalProperties.containsKey(CORE_DATA)) {
            Object coreData = additionalProperties.get(CORE_DATA);
            if(((String)coreData).equalsIgnoreCase("true")) {
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

        if(generateCoreData) {
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
        supportingFiles.add(new SupportingFile("Object-body.mustache",  coreFileFolder(), classPrefix + "Object.m"));
        supportingFiles.add(new SupportingFile("QueryParamCollection-header.mustache",  coreFileFolder(), classPrefix + "QueryParamCollection.h"));
        supportingFiles.add(new SupportingFile("QueryParamCollection-body.mustache",  coreFileFolder(), classPrefix + "QueryParamCollection.m"));
        supportingFiles.add(new SupportingFile("ApiClient-header.mustache",  coreFileFolder(), classPrefix + "ApiClient.h"));
        supportingFiles.add(new SupportingFile("ApiClient-body.mustache",  coreFileFolder(), classPrefix + "ApiClient.m"));
        supportingFiles.add(new SupportingFile("JSONRequestSerializer-body.mustache",  coreFileFolder(), classPrefix + "JSONRequestSerializer.m"));
        supportingFiles.add(new SupportingFile("JSONRequestSerializer-header.mustache",  coreFileFolder(), classPrefix + "JSONRequestSerializer.h"));
        supportingFiles.add(new SupportingFile("ResponseDeserializer-body.mustache",  coreFileFolder(), classPrefix + "ResponseDeserializer.m"));
        supportingFiles.add(new SupportingFile("ResponseDeserializer-header.mustache",  coreFileFolder(), classPrefix + "ResponseDeserializer.h"));
        supportingFiles.add(new SupportingFile("Sanitizer-body.mustache",  coreFileFolder(), classPrefix + "Sanitizer.m"));
        supportingFiles.add(new SupportingFile("Sanitizer-header.mustache",  coreFileFolder(), classPrefix + "Sanitizer.h"));
        supportingFiles.add(new SupportingFile("Logger-body.mustache",  coreFileFolder(), classPrefix + "Logger.m"));
        supportingFiles.add(new SupportingFile("Logger-header.mustache",  coreFileFolder(), classPrefix + "Logger.h"));
        supportingFiles.add(new SupportingFile("JSONValueTransformer+ISO8601-body.mustache",  coreFileFolder(), "JSONValueTransformer+ISO8601.m"));
        supportingFiles.add(new SupportingFile("JSONValueTransformer+ISO8601-header.mustache",  coreFileFolder(), "JSONValueTransformer+ISO8601.h"));
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

        if(generateCoreData) {
            supportingFiles.add(new SupportingFile("xccurrentversion.mustache", (modelPackage() + "/" + modelFilesPath + "/").replace("/", File.separator) + classPrefix + "Model.xcdatamodeld", ".xccurrentversion"));
            supportingFiles.add(new SupportingFile("Model.xcdatamodel.mustache",(modelPackage() + "/" + modelFilesPath + "/").replace("/", File.separator) + classPrefix + "Model.xcdatamodeld" + File.separator + classPrefix + "Model.xcdatamodel", "contents"));
        }
    }

    @Override
    public String toInstantiationType(Property p) {
        if (p instanceof MapProperty) {
            return instantiationTypes.get("map");
        } else if (p instanceof ArrayProperty) {
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
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;

        if (swaggerType == null) {
            swaggerType = ""; // set swagger type to empty string if null
        }

        // TODO avoid using toLowerCase as typeMapping should be case-sensitive
        if (typeMapping.containsKey(swaggerType.toLowerCase())) {
            type = typeMapping.get(swaggerType.toLowerCase());
            if (languageSpecificPrimitives.contains(type) && !foundationClasses.contains(type)) {
                return toModelNameWithoutReservedWordCheck(type);
            }
        } else {
            type = swaggerType;
        }
        return toModelNameWithoutReservedWordCheck(type);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            String innerTypeDeclaration = getTypeDeclaration(inner);
            if (innerTypeDeclaration.endsWith("*")) {
                innerTypeDeclaration = innerTypeDeclaration.substring(0, innerTypeDeclaration.length() - 1);
            }
            // In this condition, type of property p is array of primitive,
            // return container type with pointer, e.g. `NSArray*<NSString*>*'
            if (languageSpecificPrimitives.contains(innerTypeDeclaration)) {
                return getSwaggerType(p) +  "<" + innerTypeDeclaration + "*>*";
            }
            // In this condition, type of property p is array of model,
            // return container type combine inner type with pointer, e.g. `NSArray<SWGTag>*'
            else {
                for (String sd : advancedMapingTypes) {
                    if(innerTypeDeclaration.startsWith(sd)) {
                        return getSwaggerType(p) + "<" + innerTypeDeclaration + "*>*";
                    }
                }
                return getSwaggerType(p) + "<" + innerTypeDeclaration + ">*";
            }
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            String innerTypeDeclaration = getTypeDeclaration(inner);

            if (innerTypeDeclaration.endsWith("*")) {
                innerTypeDeclaration = innerTypeDeclaration.substring(0, innerTypeDeclaration.length() - 1);
            }
            if (languageSpecificPrimitives.contains(innerTypeDeclaration)) {
                return getSwaggerType(p) +  "<NSString*, " + innerTypeDeclaration + "*>*";
            } else {
                for (String s : advancedMapingTypes) {
                    if(innerTypeDeclaration.startsWith(s)) {
                        return getSwaggerType(p) + "<NSString*, " + innerTypeDeclaration + "*>*";
                    }
                }
                return getSwaggerType(p) + "<" + innerTypeDeclaration + ">*";
            }
        } else {
            String swaggerType = getSwaggerType(p);
            // In this condition, type of p is objective-c primitive type, e.g. `NSSNumber',
            // return type of p with pointer, e.g. `NSNumber*'
            if (languageSpecificPrimitives.contains(swaggerType) &&
                    foundationClasses.contains(swaggerType)) {
                return swaggerType + "*";
            }
            // In this condition, type of p is c primitive type, e.g. `bool',
            // return type of p, e.g. `bool'
            else if (languageSpecificPrimitives.contains(swaggerType)) {
                return swaggerType;
            }
            // In this condition, type of p is objective-c object type, e.g. `SWGPet',
            // return type of p with pointer, e.g. `SWGPet*'
            else {
                return swaggerType + "*";
            }
        }
    }

    @Override
    public boolean isDataTypeBinary(String dataType) {
        return dataType.toLowerCase().startsWith("nsdata");
    }

    @Override
    public String toModelName(String type) {
        // model name cannot use reserved keyword
        if (reservedWords.contains(type)) {
            LOGGER.warn(type+ " (reserved word) cannot be used as model name. Renamed to " + ("model_" + type) + " before further processing");
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
     * @param type Model anme
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
    protected void setNonArrayMapProperty(CodegenProperty property, String type) {
        super.setNonArrayMapProperty(property, type);
        if ("NSDictionary".equals(type)) {
            property.setter = "initWithDictionary";
        } else {
            property.setter = "initWithValues";
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
        return (outputFolder + "/"+ apiPackage() + "/" + apiFilesPath).replace("/", File.separator);
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + "/"+ modelPackage() + "/" + modelFilesPath).replace("/", File.separator);
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
        if(this.reservedWordsMappings().containsKey(name)) {
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
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId), true));
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
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (!operation.allParams.isEmpty()) {
                    String firstParamName = operation.allParams.get(0).paramName;
                    operation.vendorExtensions.put("firstParamAltName", camelize(firstParamName));
                }
            }
        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property){
        super.postProcessModelProperty(model,property);
        property.vendorExtensions.put("x-uppercaseName", camelize(property.name));
    }

    /**
     * Return the default value of the property
     *
     * @param p Swagger property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            StringProperty dp = (StringProperty) p;
            if (dp.getDefault() != null) {
                return "@\"" + dp.getDefault() + "\"";
            }
        } else if (p instanceof BooleanProperty) {
            BooleanProperty dp = (BooleanProperty) p;
            if (dp.getDefault() != null) {
                if (dp.getDefault().toString().equalsIgnoreCase("false"))
                    return "@(NO)";
                else
                    return "@(YES)";
            }
        } else if (p instanceof DateProperty) {
            // TODO
        } else if (p instanceof DateTimeProperty) {
            // TODO
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return "@" + dp.getDefault().toString();
            }
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getDefault() != null) {
                return "@" + dp.getDefault().toString();
            }
        } else if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getDefault() != null) {
                return "@" + dp.getDefault().toString();
            }
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getDefault() != null) {
                return "@" + dp.getDefault().toString();
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

        if ("NSString*".equalsIgnoreCase(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "@\"" + escapeText(example) + "\"";
        } else if ("NSNumber*".equals(type)) {
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
        } else if ("NSURL*".equalsIgnoreCase(type)) {
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
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            type = type.replace("*", "");
            // e.g. [[SWGPet alloc] init
            example = "[[" + type + " alloc] init]";
        } else {
            LOGGER.warn("Type " + type + " not handled properly in setParameterExampleValue");
        }

        if (example == null) {
            example = "NULL";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            example = "@[" + example + "]";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
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
