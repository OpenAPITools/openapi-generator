package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class SymfonyServerCodegen extends AbstractPhpCodegen implements CodegenConfig {
    @SuppressWarnings("hiding")
    static Logger LOGGER = LoggerFactory.getLogger(SymfonyServerCodegen.class);

    public static final String VARIABLE_NAMING_CONVENTION = "variableNamingConvention";
    public static final String BUNDLE_NAME = "bundleName";
    public static final String COMPOSER_VENDOR_NAME = "composerVendorName";
    public static final String COMPOSER_PROJECT_NAME = "composerProjectName";
    public static final Map<String, String> SYMFONY_EXCEPTIONS;
    protected String testsPackage;
    protected String apiTestsPackage;
    protected String modelTestsPackage;
    protected String composerVendorName = "swagger";
    protected String composerProjectName = "server-bundle";
    protected String testsDirName = "Tests";
    protected String bundleName;
    protected String bundleClassName;
    protected String bundleExtensionName;
    protected String bundleAlias;
    protected String controllerDirName = "Controller";
    protected String controllerPackage;

    static {
        SYMFONY_EXCEPTIONS = new HashMap<>();
        SYMFONY_EXCEPTIONS.put("400", "Symfony\\Component\\HttpKernel\\Exception\\BadRequestHttpException");
        SYMFONY_EXCEPTIONS.put("401", "Symfony\\Component\\HttpKernel\\Exception\\UnauthorizedHttpException");
        SYMFONY_EXCEPTIONS.put("403", "Symfony\\Component\\HttpKernel\\Exception\\AccessDeniedHttpException");
        SYMFONY_EXCEPTIONS.put("404", "Symfony\\Component\\HttpKernel\\Exception\\NotFoundHttpException");
        SYMFONY_EXCEPTIONS.put("405", "Symfony\\Component\\HttpKernel\\Exception\\MethodNotAllowedHttpException");
        SYMFONY_EXCEPTIONS.put("406", "Symfony\\Component\\HttpKernel\\Exception\\NotAcceptableHttpException");
        SYMFONY_EXCEPTIONS.put("409", "Symfony\\Component\\HttpKernel\\Exception\\ConflictHttpException");
        SYMFONY_EXCEPTIONS.put("410", "Symfony\\Component\\HttpKernel\\Exception\\GoneHttpException");
        SYMFONY_EXCEPTIONS.put("411", "Symfony\\Component\\HttpKernel\\Exception\\LengthRequiredHttpException");
        SYMFONY_EXCEPTIONS.put("412", "Symfony\\Component\\HttpKernel\\Exception\\PreconditionFailedHttpException");
        SYMFONY_EXCEPTIONS.put("415", "Symfony\\Component\\HttpKernel\\Exception\\UnsupportedMediaTypeHttpException");
        SYMFONY_EXCEPTIONS.put("422", "Symfony\\Component\\HttpKernel\\Exception\\UnprocessableEntityHttpException");
        SYMFONY_EXCEPTIONS.put("428", "Symfony\\Component\\HttpKernel\\Exception\\PreconditionRequiredHttpException");
        SYMFONY_EXCEPTIONS.put("429", "Symfony\\Component\\HttpKernel\\Exception\\TooManyRequestsHttpException");
        SYMFONY_EXCEPTIONS.put("503", "Symfony\\Component\\HttpKernel\\Exception\\ServiceUnavailableHttpException");
    }

    public SymfonyServerCodegen() {
        super();

        // clear import mapping (from default generator) as php does not use it
        // at the moment
        importMapping.clear();

        supportsInheritance = true;
        srcBasePath = ".";
        setInvokerPackage("Swagger\\Server");
        setBundleName("SwaggerServer");
        packagePath = "SymfonyBundle-php";
        modelDirName = "Model";
        docsBasePath = "Resources/docs";
        apiDocPath = docsBasePath + "/" + apiDirName;
        modelDocPath = docsBasePath + "/" + modelDirName;
        outputFolder = "generated-code" + File.separator + "php";
        apiTemplateFiles.put("api_controller.mustache", ".php");
        modelTestTemplateFiles.put("model_test.mustache", ".php");
        embeddedTemplateDir = templateDir = "php-symfony";

        setReservedWordsLowerCase(
                Arrays.asList(
                    // local variables used in api methods (endpoints)
                    "resourcePath", "httpBody", "queryParams", "headerParams",
                    "formParams", "_header_accept", "_tempBody",

                    // PHP reserved words
                    "__halt_compiler", "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class", "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else", "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile", "eval", "exit", "extends", "final", "for", "foreach", "function", "global", "goto", "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset", "list", "namespace", "new", "or", "print", "private", "protected", "public", "require", "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use", "var", "while", "xor")
        );

        // ref: http://php.net/manual/en/language.types.intro.php
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "bool",
                        "boolean",
                        "int",
                        "integer",
                        "double",
                        "float",
                        "string",
                        "object",
                        "DateTime",
                        "mixed",
                        "number",
                        "void",
                        "byte")
        );

        instantiationTypes.put("array", "array");
        instantiationTypes.put("map", "map");


        // provide primitives to mustache template
        List sortedLanguageSpecificPrimitives= new ArrayList(languageSpecificPrimitives);
        Collections.sort(sortedLanguageSpecificPrimitives);
        String primitives = "'" + StringUtils.join(sortedLanguageSpecificPrimitives, "', '") + "'";
        additionalProperties.put("primitives", primitives);

        // ref: https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#data-types
        typeMapping = new HashMap<String, String>();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");
        typeMapping.put("number", "float");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("string", "string");
        typeMapping.put("byte", "int");
        typeMapping.put("boolean", "bool");
        typeMapping.put("Date", "\\DateTime");
        typeMapping.put("DateTime", "\\DateTime");
        typeMapping.put("file", "\\SplFileObject");
        typeMapping.put("map", "map");
        typeMapping.put("array", "array");
        typeMapping.put("list", "array");
        typeMapping.put("object", "object");
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");

        cliOptions.add(new CliOption(COMPOSER_VENDOR_NAME, "The vendor name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. yaypets. IMPORTANT NOTE (2016/03): composerVendorName will be deprecated and replaced by gitUserId in the next swagger-codegen release"));
        cliOptions.add(new CliOption(BUNDLE_NAME, "The name of the Symfony bundle. The template uses {{bundleName}}"));
        cliOptions.add(new CliOption(COMPOSER_PROJECT_NAME, "The project name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. petstore-client. IMPORTANT NOTE (2016/03): composerProjectName will be deprecated and replaced by gitRepoId in the next swagger-codegen release"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "hides the timestamp when files were generated")
                .defaultValue(Boolean.TRUE.toString()));
    }

    public String getBundleName() {
        return bundleName;
    }

    public void setBundleName(String bundleName) {
        this.bundleName = bundleName;
        this.bundleClassName = bundleName + "Bundle";
        this.bundleExtensionName = bundleName + "Extension";
        this.bundleAlias = snakeCase(bundleName).replaceAll("([A-Z]+)", "\\_$1").toLowerCase();
    }


    public String controllerFileFolder() {
        return (outputFolder + "/" + toPackagePath(controllerPackage, srcBasePath));
    }

    @Override
    public String escapeText(String input) {
        if (input != null) {
            // Trim the string to avoid leading and trailing spaces.
            return super.escapeText(input).trim();
        }
        return input;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "php-symfony";
    }

    @Override
    public String getHelp() {
        return "Generates a Symfony server bundle.";
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        if (templateName.equals("api_controller.mustache"))
            return controllerFileFolder() + '/' + toControllerName(tag) + suffix;

        return apiFileFolder() + '/' + toApiFilename(tag) + suffix;
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

        if (additionalProperties.containsKey(BUNDLE_NAME)) {
            this.setBundleName((String) additionalProperties.get(BUNDLE_NAME));
        } else {
            additionalProperties.put(BUNDLE_NAME, bundleName);
        }

        if (additionalProperties.containsKey(COMPOSER_PROJECT_NAME)) {
            this.setComposerProjectName((String) additionalProperties.get(COMPOSER_PROJECT_NAME));
        } else {
            additionalProperties.put(COMPOSER_PROJECT_NAME, composerProjectName);
        }

        if (additionalProperties.containsKey(COMPOSER_VENDOR_NAME)) {
            this.setComposerVendorName((String) additionalProperties.get(COMPOSER_VENDOR_NAME));
        } else {
            additionalProperties.put(COMPOSER_VENDOR_NAME, composerVendorName);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_VERSION)) {
            this.setArtifactVersion((String) additionalProperties.get(CodegenConstants.ARTIFACT_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }

        if (additionalProperties.containsKey(VARIABLE_NAMING_CONVENTION)) {
            this.setParameterNamingConvention((String) additionalProperties.get(VARIABLE_NAMING_CONVENTION));
        }

        additionalProperties.put("escapedInvokerPackage", invokerPackage.replace("\\", "\\\\"));
        additionalProperties.put("controllerPackage", controllerPackage);
        additionalProperties.put("apiTestsPackage", apiTestsPackage);
        additionalProperties.put("modelTestsPackage", modelTestsPackage);

        // make Symonfy-specific properties available
        additionalProperties.put("bundleName", bundleName);
        additionalProperties.put("bundleClassName", bundleClassName);
        additionalProperties.put("bundleExtensionName", bundleExtensionName);
        additionalProperties.put("bundleAlias", bundleAlias);

        // make api and model src path available in mustache template
        additionalProperties.put("apiSrcPath", "./" + toSrcPath(apiPackage, srcBasePath));
        additionalProperties.put("modelSrcPath", "./" + toSrcPath(modelPackage, srcBasePath));
        additionalProperties.put("testsSrcPath", "./" + toSrcPath(testsPackage, srcBasePath));
        additionalProperties.put("apiTestsSrcPath", "./" + toSrcPath(apiTestsPackage, srcBasePath));
        additionalProperties.put("modelTestsSrcPath", "./" + toSrcPath(modelTestsPackage, srcBasePath));
        additionalProperties.put("apiTestPath", "./" + testsDirName + "/" + apiDirName);
        additionalProperties.put("modelTestPath", "./" + testsDirName + "/" + modelDirName);

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        // make test path available in mustache template
        additionalProperties.put("testsDirName", testsDirName);

        supportingFiles.add(new SupportingFile("Controller.mustache", toPackagePath(controllerPackage, srcBasePath), "Controller.php"));
        supportingFiles.add(new SupportingFile("Bundle.mustache", getPackagePath(), bundleClassName + ".php"));
        supportingFiles.add(new SupportingFile("Extension.mustache", getPackagePath() + "/DependencyInjection", bundleExtensionName + ".php"));
        supportingFiles.add(new SupportingFile("ApiPass.mustache", getPackagePath() + "/DependencyInjection/Compiler", bundleName + "ApiPass.php"));
        supportingFiles.add(new SupportingFile("ApiServer.mustache", toPackagePath(apiPackage, srcBasePath), "ApiServer.php"));
        supportingFiles.add(new SupportingFile("ModelSerializer.mustache", toPackagePath(modelPackage, srcBasePath), "ModelSerializer.php"));
        supportingFiles.add(new SupportingFile("ModelInterface.mustache", toPackagePath(modelPackage, srcBasePath), "ModelInterface.php"));
        supportingFiles.add(new SupportingFile("routing.mustache", getPackagePath() + "/Resources/config", "routing.yml"));
        supportingFiles.add(new SupportingFile("services.mustache", getPackagePath() + "/Resources/config", "services.yml"));
        supportingFiles.add(new SupportingFile("composer.mustache", getPackagePath(), "composer.json"));
        supportingFiles.add(new SupportingFile("autoload.mustache", getPackagePath(), "autoload.php"));
        supportingFiles.add(new SupportingFile("README.mustache", getPackagePath(), "README.md"));
        supportingFiles.add(new SupportingFile("phpunit.xml.mustache", getPackagePath(), "phpunit.xml.dist"));
        supportingFiles.add(new SupportingFile(".travis.yml", getPackagePath(), ".travis.yml"));
        supportingFiles.add(new SupportingFile(".php_cs", getPackagePath(), ".php_cs"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", getPackagePath(), "git_push.sh"));
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        objs = super.postProcessOperations(objs);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        operations.put("controllerName", toControllerName((String) operations.get("pathPrefix")));
        operations.put("symfonyService", toSymfonyService((String) operations.get("pathPrefix")));

        HashSet<String> imports = new HashSet<>();
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            for (CodegenParameter param : op.allParams) {
                final String simpleName = extractSimpleName(param.dataType);
                param.vendorExtensions.put("x-simpleName", simpleName);
                final boolean isScalarType = typeMapping.containsValue(param.dataType);
                param.vendorExtensions.put("x-parameterType", isScalarType ? null : simpleName);
                if (!isScalarType) {
                    imports.add(param.dataType);
                }
            }

            for (CodegenResponse response : op.responses) {
                final String exception = SYMFONY_EXCEPTIONS.get(response.code);
                response.vendorExtensions.put("x-symfonyException", exception);
                response.vendorExtensions.put("x-symfonyExceptionSimple", extractSimpleName(exception));

                // Add simple return type to response
                if (response.dataType != null) {
                    final String dataType = extractSimpleName(response.dataType);
                    response.vendorExtensions.put("x-simpleName", dataType);
                    imports.add(response.dataType.replaceFirst("\\[\\]$", ""));
                }

                if (exception != null) {
                    imports.add(exception);
                }
            }
        }

        operations.put("imports", new ArrayList<>(imports));

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = super.postProcessModels(objs);

        ArrayList<Object> modelsArray = (ArrayList<Object>) objs.get("models");
        Map<String, Object> models = (Map<String, Object>) modelsArray.get(0);
        CodegenModel model = (CodegenModel) models.get("model");
        HashSet<String> imports = new HashSet<>();

        // Simplify model var type
        for (CodegenProperty var : model.vars) {
            if (var.datatype != null) {
                final String importType = var.datatype.replaceFirst("\\[\\]$", "");
                final String dataType = extractSimpleName(var.datatype);
                final boolean isScalarType = typeMapping.containsValue(importType);
                if (!isScalarType) {
                    var.vendorExtensions.put("x-typeAnnotation", dataType.endsWith("[]") ? "array" : dataType);
                    imports.add(importType);
                    var.datatype = dataType;
                }
            }
        }

        objs.put("useStatements", new ArrayList<>(imports));

        return objs;
    }

    @Override
    public String escapeReservedWord(String name) {
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiTestFileFolder() {
        return (outputFolder + "/" + toPackagePath(apiTestsPackage, srcBasePath));
    }

    @Override
    public String modelTestFileFolder() {
        return (outputFolder + "/" + toPackagePath(modelTestsPackage, srcBasePath));
    }

    public void setComposerVendorName(String composerVendorName) {
        this.composerVendorName = composerVendorName;
    }

    public void setComposerProjectName(String composerProjectName) {
        this.composerProjectName = composerProjectName;
    }

    @Override
    public void setInvokerPackage(String invokerPackage) {
        super.setInvokerPackage(invokerPackage);
        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;
        testsPackage = invokerPackage + "\\" + testsDirName;
        apiTestsPackage = testsPackage + "\\" + apiDirName;
        modelTestsPackage = testsPackage + "\\" + modelDirName;
        controllerPackage = invokerPackage + "\\" + controllerDirName;
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getTypeDeclaration(inner) + "[]";
        }

        if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "array<string," + getTypeDeclaration(inner) + ">";
        }
        
        if (p instanceof RefProperty) {
            return getTypeDeclaration(getPropertyTypeDeclaration(p));
        }
        
        return getPropertyTypeDeclaration(p);
    }

    /**
     * Output the type declaration of the property
     *
     * @param p Swagger Property object
     * @return a string presentation of the property type
     */
    public String getPropertyTypeDeclaration(Property p) {
        String swaggerType = getSwaggerType(p);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }
        return swaggerType;
    }

    @Override
    public String getTypeDeclaration(String name) {
        if (!languageSpecificPrimitives.contains(name)) {
            return modelPackage + "\\" + name;
        }
        return super.getTypeDeclaration(name);
    }

    public String toApiName(String name) {
        if (name.isEmpty()) {
            return "DefaultApiInterface";
        }
        return initialCaps(name) + "ApiInterface";
    }

    protected String toControllerName(String name) {
        if (name.isEmpty()) {
            return "DefaultController";
        }
        return initialCaps(name) + "Controller";
    }

    protected String toSymfonyService(String name) {
        String prefix = composerVendorName + ".api.";
        if (name.isEmpty()) {
            return prefix + "default";
        }

        return prefix + name;
    }
}
