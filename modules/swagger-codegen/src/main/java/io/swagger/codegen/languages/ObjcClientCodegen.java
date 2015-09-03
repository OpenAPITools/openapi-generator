package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang.StringUtils;

public class ObjcClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected Set<String> foundationClasses = new HashSet<String>();
    protected String podName = "SwaggerClient";
    protected String podVersion = "1.0.0";
    protected String classPrefix = "SWG";
    protected String authorName = "Swagger";
    protected String authorEmail = "apiteam@swagger.io";
    protected String license = "MIT";
    protected String gitRepoURL = "https://github.com/swagger-api/swagger-codegen";

    public ObjcClientCodegen() {
        super();
        
        outputFolder = "generated-code" + File.separator + "objc";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".m");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-body.mustache", ".m");
        templateDir = "objc";

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
        
        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("NSNumber");
        languageSpecificPrimitives.add("NSString");
        languageSpecificPrimitives.add("NSObject");
        languageSpecificPrimitives.add("NSDate");
        languageSpecificPrimitives.add("NSURL");
        languageSpecificPrimitives.add("bool");
        languageSpecificPrimitives.add("BOOL");

        typeMapping.clear();
        typeMapping.put("enum", "NSString");
        typeMapping.put("date", "NSDate");
        typeMapping.put("DateTime", "NSDate");
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
        typeMapping.put("List", "NSArray");
        typeMapping.put("object", "NSObject");
        typeMapping.put("file", "NSURL");

        // ref: http://www.tutorialspoint.com/objective_c/objective_c_basic_syntax.htm
        reservedWords = new HashSet<String>(
                Arrays.asList(
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
                        "NSURL",
                        "NSDictionary")
        );

        instantiationTypes.put("array", "NSMutableArray");
        instantiationTypes.put("map", "NSMutableDictionary");

        cliOptions.clear();
        cliOptions.add(new CliOption("classPrefix", "prefix for generated classes (convention: Abbreviation of pod name e.g. `HN` for `HackerNews`), default: `SWG`"));
        cliOptions.add(new CliOption("podName", "cocoapods package name (convention: CameCase), default: `SwaggerClient`"));
        cliOptions.add(new CliOption("podVersion", "cocoapods package version, default: `1.0.0`"));
        cliOptions.add(new CliOption("authorName", "Name to use in the podspec file, default: `Swagger`"));
        cliOptions.add(new CliOption("authorEmail", "Email to use in the podspec file, default: `apiteam@swagger.io`"));
        cliOptions.add(new CliOption("gitRepoURL", "URL for the git repo where this podspec should point to, default: `https://github.com/swagger-api/swagger-codegen`"));
        cliOptions.add(new CliOption("license", "License to use in the podspec file, default: `MIT`"));
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "objc";
    }

    public String getHelp() {
        return "Generates an Objective-C client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("podName")) {
            setPodName((String) additionalProperties.get("podName"));
        }

        if (additionalProperties.containsKey("podVersion")) {
            setPodVersion((String) additionalProperties.get("podVersion"));
        }

        if (additionalProperties.containsKey("classPrefix")) {
            setClassPrefix((String) additionalProperties.get("classPrefix"));
        }
        
        if (additionalProperties.containsKey("authorName")) {
            setAuthorName((String) additionalProperties.get("authorName"));
        }
        
        if (additionalProperties.containsKey("authorEmail")) {
            setAuthorEmail((String) additionalProperties.get("authorEmail"));
        }
        
        if (additionalProperties.containsKey("gitRepoURL")) {
            setGitRepoURL((String) additionalProperties.get("gitRepoURL"));
        }
        
        if (additionalProperties.containsKey("license")) {
            setLicense((String) additionalProperties.get("license"));
        }

        additionalProperties.put("podName", podName);
        additionalProperties.put("podVersion", podVersion);
        additionalProperties.put("classPrefix", classPrefix);
        additionalProperties.put("authorName", authorName);
        additionalProperties.put("authorEmail", authorEmail);
        additionalProperties.put("gitRepoURL", gitRepoURL);
        additionalProperties.put("license", license);

        String swaggerFolder = podName;

        modelPackage = swaggerFolder;
        apiPackage = swaggerFolder;

        supportingFiles.add(new SupportingFile("Object-header.mustache", swaggerFolder, classPrefix + "Object.h"));
        supportingFiles.add(new SupportingFile("Object-body.mustache", swaggerFolder, classPrefix + "Object.m"));
        supportingFiles.add(new SupportingFile("QueryParamCollection-header.mustache", swaggerFolder, classPrefix + "QueryParamCollection.h"));
        supportingFiles.add(new SupportingFile("QueryParamCollection-body.mustache", swaggerFolder, classPrefix + "QueryParamCollection.m"));
        supportingFiles.add(new SupportingFile("ApiClient-header.mustache", swaggerFolder, classPrefix + "ApiClient.h"));
        supportingFiles.add(new SupportingFile("ApiClient-body.mustache", swaggerFolder, classPrefix + "ApiClient.m"));
        supportingFiles.add(new SupportingFile("JSONResponseSerializer-header.mustache", swaggerFolder, classPrefix + "JSONResponseSerializer.h"));
        supportingFiles.add(new SupportingFile("JSONResponseSerializer-body.mustache", swaggerFolder, classPrefix + "JSONResponseSerializer.m"));
        supportingFiles.add(new SupportingFile("JSONRequestSerializer-body.mustache", swaggerFolder, classPrefix + "JSONRequestSerializer.m"));
        supportingFiles.add(new SupportingFile("JSONRequestSerializer-header.mustache", swaggerFolder, classPrefix + "JSONRequestSerializer.h"));
        supportingFiles.add(new SupportingFile("JSONValueTransformer+ISO8601.m", swaggerFolder, "JSONValueTransformer+ISO8601.m"));
        supportingFiles.add(new SupportingFile("JSONValueTransformer+ISO8601.h", swaggerFolder, "JSONValueTransformer+ISO8601.h"));
        supportingFiles.add(new SupportingFile("Configuration-body.mustache", swaggerFolder, classPrefix + "Configuration.m"));
        supportingFiles.add(new SupportingFile("Configuration-header.mustache", swaggerFolder, classPrefix + "Configuration.h"));
        supportingFiles.add(new SupportingFile("podspec.mustache", "", podName + ".podspec"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    public String toInstantiationType(Property p) {
        if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            String inner = getSwaggerType(ap.getAdditionalProperties());
            return instantiationTypes.get("map");
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
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
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type) && !foundationClasses.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            String innerType = getSwaggerType(inner);

            String innerTypeDeclaration = getTypeDeclaration(inner);

            if (innerTypeDeclaration.endsWith("*")) {
                innerTypeDeclaration = innerTypeDeclaration.substring(0, innerTypeDeclaration.length() - 1);
            }

            // In this codition, type of property p is array of primitive,
            // return container type with pointer, e.g. `NSArray* /* NSString */'
            if (languageSpecificPrimitives.contains(innerType)) {
                return getSwaggerType(p) + "*" + " /* " + innerTypeDeclaration + " */";
            }
            // In this codition, type of property p is array of model,
            // return container type combine inner type with pointer, e.g. `NSArray<SWGTag>*'
            else {
                return getSwaggerType(p) + "<" + innerTypeDeclaration + ">*";
            }
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            String innerTypeDeclaration = getTypeDeclaration(inner);

            if (innerTypeDeclaration.endsWith("*")) {
                innerTypeDeclaration = innerTypeDeclaration.substring(0, innerTypeDeclaration.length() - 1);
            }
            return getSwaggerType(p) + "* /* NSString, " + innerTypeDeclaration + " */";
        } else {
            String swaggerType = getSwaggerType(p);

            // In this codition, type of p is objective-c primitive type, e.g. `NSSNumber',
            // return type of p with pointer, e.g. `NSNumber*'
            if (languageSpecificPrimitives.contains(swaggerType) &&
                    foundationClasses.contains(swaggerType)) {
                return swaggerType + "*";
            }
            // In this codition, type of p is c primitive type, e.g. `bool',
            // return type of p, e.g. `bool'
            else if (languageSpecificPrimitives.contains(swaggerType)) {
                return swaggerType;
            }
            // In this codition, type of p is objective-c object type, e.g. `SWGPet',
            // return type of p with pointer, e.g. `SWGPet*'
            else {
                return swaggerType + "*";
            }
        }
    }

    @Override
    public String toModelName(String type) {
        type = type.replaceAll("[^0-9a-zA-Z_]", "_");

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
            return classPrefix + camelize(type);
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
    public String toDefaultValue(Property p) {
        return null;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separatorChar + apiPackage();
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separatorChar + modelPackage();
    }

    @Override
    public String toApiName(String name) {
        return classPrefix + camelize(name) + "Api";
    }

    public String toApiFilename(String name) {
        return classPrefix + camelize(name) + "Api";
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all upper case, do noting
        if (name.matches("^[A-Z_]$")) {
            return name;
        }

        // camelize (lower first character) the variable name
        // e.g. `pet_id` to `petId`
        name = camelize(name, true);

        // for reserved word or word starting with number, prepend `_`
        if (reservedWords.contains(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
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
}
