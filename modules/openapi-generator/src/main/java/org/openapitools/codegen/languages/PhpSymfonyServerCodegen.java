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

public class PhpSymfonyServerCodegen extends AbstractPhpCodegen implements CodegenConfig {
    @SuppressWarnings("hiding")
    private final Logger LOGGER = LoggerFactory.getLogger(PhpSymfonyServerCodegen.class);

    public static final String BUNDLE_NAME = "bundleName";
    public static final String BUNDLE_ALIAS = "bundleAlias";
    public static final String COMPOSER_VENDOR_NAME = "composerVendorName";
    public static final String COMPOSER_PROJECT_NAME = "composerProjectName";
    public static final String PHP_LEGACY_SUPPORT = "phpLegacySupport";
    public static final Map<String, String> SYMFONY_EXCEPTIONS;
    protected String testsPackage;
    protected String apiTestsPackage;
    protected String modelTestsPackage;
    protected String composerVendorName = "openapi";
    protected String composerProjectName = "server-bundle";
    protected String testsDirName = "Tests";
    protected String bundleName;
    protected String bundleClassName;
    protected String bundleExtensionName;
    protected String bundleAlias;
    protected String controllerDirName = "Controller";
    protected String serviceDirName = "Service";
    protected String controllerPackage;
    protected String controllerTestsPackage;
    protected String servicePackage;
    protected Boolean phpLegacySupport = Boolean.TRUE;

    protected HashSet<String> typeHintable;

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

    public PhpSymfonyServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
        );

        // clear import mapping (from default generator) as php does not use it
        // at the moment
        importMapping.clear();

        supportsInheritance = true;
        srcBasePath = ".";
        setInvokerPackage("OpenAPI\\Server");
        setBundleName("OpenAPIServer");
        setBundleAlias("open_api_server");
        modelDirName = "Model";
        docsBasePath = "Resources" + "/" + "docs";
        apiDocPath = docsBasePath + "/" + apiDirName;
        modelDocPath = docsBasePath + "/" + modelDirName;
        outputFolder = "generated-code" + File.separator + "php";
        apiTemplateFiles.put("api_controller.mustache", ".php");
        modelTestTemplateFiles.put("testing/model_test.mustache", ".php");
        apiTestTemplateFiles = new HashMap<>();
        apiTestTemplateFiles.put("testing/api_test.mustache", ".php");
        embeddedTemplateDir = templateDir = "php-symfony";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        setReservedWordsLowerCase(
                Arrays.asList(
                        // local variables used in api methods (endpoints)
                        "resourcePath", "httpBody", "queryParams", "headerParams",
                        "formParams", "_header_accept", "_tempBody",

                        // PHP reserved words
                        "__halt_compiler", "abstract", "and", "array", "as", "break", "callable", "case", "catch",
                        "class", "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else",
                        "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile",
                        "eval", "exit", "extends", "final", "for", "foreach", "function", "global", "goto", "if",
                        "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset",
                        "list", "namespace", "new", "or", "print", "private", "protected", "public", "require",
                        "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use",
                        "var", "while", "xor"
                )
        );

        // ref: http://php.net/manual/en/language.types.intro.php
        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "bool",
                        "int",
                        "double",
                        "float",
                        "string",
                        "object",
                        "mixed",
                        "number",
                        "void",
                        "byte",
                        "array",
                        "\\DateTime",
                        "UploadedFile"
                )
        );

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "\\DateTime",
                        "UploadedFile"
                )
        );

        variableNamingConvention = "camelCase";

        // provide primitives to mustache template
        List<String> sortedLanguageSpecificPrimitives = new ArrayList<>(languageSpecificPrimitives);
        Collections.sort(sortedLanguageSpecificPrimitives);
        String primitives = "'" + StringUtils.join(sortedLanguageSpecificPrimitives, "', '") + "'";
        additionalProperties.put("primitives", primitives);

        // ref: https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#data-types
        typeMapping = new HashMap<>();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");
        typeMapping.put("decimal", "float");
        typeMapping.put("number", "float");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("string", "string");
        typeMapping.put("byte", "int");
        typeMapping.put("boolean", "bool");
        typeMapping.put("Date", "\\DateTime");
        typeMapping.put("DateTime", "\\DateTime");
        typeMapping.put("file", "UploadedFile");
        typeMapping.put("map", "array");
        typeMapping.put("array", "array");
        typeMapping.put("list", "array");
        typeMapping.put("object", "array");
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");

        cliOptions.add(new CliOption(COMPOSER_VENDOR_NAME, "The vendor name used in the composer package name." +
                " The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. yaypets"));
        cliOptions.add(new CliOption(BUNDLE_NAME, "The name of the Symfony bundle. The template uses {{bundleName}}"));
        cliOptions.add(new CliOption(BUNDLE_ALIAS, "The alias of the Symfony bundle. The template uses {{aliasName}}"));
        cliOptions.add(new CliOption(COMPOSER_PROJECT_NAME, "The project name used in the composer package name." +
                " The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. petstore-client"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(PHP_LEGACY_SUPPORT, "Should the generated code be compatible with PHP 5.x?").defaultValue(Boolean.TRUE.toString()));
    }

    public String getBundleName() {
        return bundleName;
    }

    public void setBundleName(String bundleName) {
        this.bundleName = bundleName;
        this.bundleClassName = bundleName + "Bundle";
        this.bundleExtensionName = bundleName + "Extension";
    }

    public void setBundleAlias(String alias) {
        if (alias != null && !alias.isEmpty()) {
            this.bundleAlias = alias.toLowerCase(Locale.ROOT);
        } else {
            this.bundleAlias = lowerCamelCase(bundleName).replaceAll("([A-Z]+)", "\\_$1").toLowerCase(Locale.ROOT);
        }
    }

    public void setPhpLegacySupport(Boolean support) {
        this.phpLegacySupport = support;
    }

    public String controllerFileFolder() {
        return (outputFolder + File.separator + toSrcPath(controllerPackage, srcBasePath));
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
        return "Generates a PHP Symfony server bundle.";
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        if ("api_controller.mustache".equals(templateName))
            return controllerFileFolder() + File.separator + toControllerName(tag) + suffix;

        return apiFileFolder() + File.separator + toApiFilename(tag) + suffix;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(BUNDLE_NAME)) {
            this.setBundleName((String) additionalProperties.get(BUNDLE_NAME));
        } else {
            additionalProperties.put(BUNDLE_NAME, bundleName);
        }

        if (additionalProperties.containsKey(BUNDLE_ALIAS)) {
            this.setBundleAlias((String) additionalProperties.get(BUNDLE_ALIAS));
        } else {
            additionalProperties.put(BUNDLE_ALIAS, bundleAlias);
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

        if (additionalProperties.containsKey(PHP_LEGACY_SUPPORT)) {
            this.setPhpLegacySupport(Boolean.valueOf((String) additionalProperties.get(PHP_LEGACY_SUPPORT)));
        } else {
            additionalProperties.put(PHP_LEGACY_SUPPORT, phpLegacySupport);
        }

        additionalProperties.put("escapedInvokerPackage", invokerPackage.replace("\\", "\\\\"));
        additionalProperties.put("controllerPackage", controllerPackage);
        additionalProperties.put("servicePackage", servicePackage);
        additionalProperties.put("testsPackage", testsPackage);
        additionalProperties.put("apiTestsPackage", apiTestsPackage);
        additionalProperties.put("modelTestsPackage", modelTestsPackage);
        additionalProperties.put("controllerTestsPackage", controllerTestsPackage);

        // make Symfony-specific properties available
        additionalProperties.put("bundleName", bundleName);
        additionalProperties.put("bundleClassName", bundleClassName);
        additionalProperties.put("bundleExtensionName", bundleExtensionName);
        additionalProperties.put("bundleAlias", bundleAlias);

        // make api and model src path available in mustache template
        additionalProperties.put("apiSrcPath", "." + "/" + toSrcPath(apiPackage, srcBasePath));
        additionalProperties.put("modelSrcPath", "." + "/" + toSrcPath(modelPackage, srcBasePath));
        additionalProperties.put("controllerSrcPath", "." + "/" + toSrcPath(controllerPackage, srcBasePath));
        additionalProperties.put("testsSrcPath", "." + "/" + toSrcPath(testsPackage, srcBasePath));
        additionalProperties.put("apiTestsSrcPath", "." + "/" + toSrcPath(apiTestsPackage, srcBasePath));
        additionalProperties.put("modelTestsSrcPath", "." + "/" + toSrcPath(modelTestsPackage, srcBasePath));
        additionalProperties.put("apiTestPath", "." + "/" + testsDirName + "/" + apiDirName);
        additionalProperties.put("modelTestPath", "." + "/" + testsDirName + "/" + modelDirName);
        additionalProperties.put("controllerTestPath", "." + "/" + testsDirName + "/" + controllerDirName);

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        // make test path available in mustache template
        additionalProperties.put("testsDirName", testsDirName);

        final String configDir = "Resources" + File.separator + "config";
        final String dependencyInjectionDir = "DependencyInjection";

        supportingFiles.add(new SupportingFile("Controller.mustache", toSrcPath(controllerPackage, srcBasePath), "Controller.php"));
        supportingFiles.add(new SupportingFile("Bundle.mustache", "", bundleClassName + ".php"));
        supportingFiles.add(new SupportingFile("Extension.mustache", dependencyInjectionDir, bundleExtensionName + ".php"));
        supportingFiles.add(new SupportingFile("ApiPass.mustache", dependencyInjectionDir + File.separator + "Compiler", bundleName + "ApiPass.php"));
        supportingFiles.add(new SupportingFile("ApiServer.mustache", toSrcPath(apiPackage, srcBasePath), "ApiServer.php"));

        // Serialization components
        supportingFiles.add(new SupportingFile("serialization/SerializerInterface.mustache", toSrcPath(servicePackage, srcBasePath), "SerializerInterface.php"));
        supportingFiles.add(new SupportingFile("serialization/JmsSerializer.mustache", toSrcPath(servicePackage, srcBasePath), "JmsSerializer.php"));
        supportingFiles.add(new SupportingFile("serialization/StrictJsonDeserializationVisitor.mustache", toSrcPath(servicePackage, srcBasePath), "StrictJsonDeserializationVisitor.php"));
        supportingFiles.add(new SupportingFile("serialization/TypeMismatchException.mustache", toSrcPath(servicePackage, srcBasePath), "TypeMismatchException.php"));
        // Validation components
        supportingFiles.add(new SupportingFile("validation/ValidatorInterface.mustache", toSrcPath(servicePackage, srcBasePath), "ValidatorInterface.php"));
        supportingFiles.add(new SupportingFile("validation/SymfonyValidator.mustache", toSrcPath(servicePackage, srcBasePath), "SymfonyValidator.php"));

        // Testing components
        supportingFiles.add(new SupportingFile("testing/phpunit.xml.mustache", "", "phpunit.xml.dist"));
        supportingFiles.add(new SupportingFile("testing/pom.xml", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("testing/AppKernel.mustache", toSrcPath(testsPackage, srcBasePath), "AppKernel.php"));
        supportingFiles.add(new SupportingFile("testing/ControllerTest.mustache", toSrcPath(controllerTestsPackage, srcBasePath), "ControllerTest.php"));
        supportingFiles.add(new SupportingFile("testing/test_config.yml", toSrcPath(testsPackage, srcBasePath), "test_config.yml"));

        supportingFiles.add(new SupportingFile("routing.mustache", configDir, "routing.yml"));
        supportingFiles.add(new SupportingFile("services.mustache", configDir, "services.yml"));
        supportingFiles.add(new SupportingFile("composer.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("autoload.mustache", "", "autoload.php"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile(".php_cs.dist", "", ".php_cs.dist"));
        supportingFiles.add(new SupportingFile(".coveralls.yml", "", ".coveralls.yml"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));

        // Type-hintable primitive types
        // ref: http://php.net/manual/en/functions.arguments.php#functions.arguments.type-declaration
        if (phpLegacySupport) {
            typeHintable = new HashSet<>(
                    Arrays.asList(
                            "array"
                    )
            );
        } else {
            typeHintable = new HashSet<>(
                    Arrays.asList(
                            "array",
                            "bool",
                            "float",
                            "int",
                            "string"
                    )
            );
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        operations.put("controllerName", toControllerName((String) operations.get("pathPrefix")));
        operations.put("symfonyService", toSymfonyService((String) operations.get("pathPrefix")));

        List<CodegenSecurity> authMethods = new ArrayList<>();
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        for (CodegenOperation op : operationList) {
            // Loop through all input parameters to determine, whether we have to import something to
            // make the input type available.
            for (CodegenParameter param : op.allParams) {
                // Determine if the parameter type is supported as a type hint and make it available
                // to the templating engine
                String typeHint = getTypeHint(param.dataType);
                if (!typeHint.isEmpty()) {
                    param.vendorExtensions.put("x-parameter-type", typeHint);
                }

                if (param.isContainer) {
                    param.vendorExtensions.put("x-parameter-type", getTypeHint(param.dataType + "[]"));
                }

                // Create a variable to display the correct data type in comments for interfaces
                param.vendorExtensions.put("x-comment-type", "\\" + param.dataType);
                if (param.isContainer) {
                    param.vendorExtensions.put("x-comment-type", "\\" + param.dataType + "[]");
                }
            }

            // Create a variable to display the correct return type in comments for interfaces
            if (op.returnType != null) {
                op.vendorExtensions.put("x-comment-type", "\\" + op.returnType);
                if ("array".equals(op.returnContainer)) {
                    op.vendorExtensions.put("x-comment-type", "\\" + op.returnType + "[]");
                }
            } else {
                op.vendorExtensions.put("x-comment-type", "void");
            }

            // Add operation's authentication methods to whole interface
            if (op.authMethods != null) {
                for (CodegenSecurity am : op.authMethods) {
                    if (!authMethods.contains(am)) {
                        authMethods.add(am);
                    }
                }
            }
        }

        operations.put("authMethods", authMethods);

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = super.postProcessModels(objs);

        ArrayList<Object> modelsArray = (ArrayList<Object>) objs.get("models");
        Map<String, Object> models = (Map<String, Object>) modelsArray.get(0);
        CodegenModel model = (CodegenModel) models.get("model");

        // Simplify model var type
        for (CodegenProperty var : model.vars) {
            if (var.dataType != null) {
                // Determine if the parameter type is supported as a type hint and make it available
                // to the templating engine
                String typeHint = getTypeHint(var.dataType);
                if (!typeHint.isEmpty()) {
                    var.vendorExtensions.put("x-parameter-type", typeHint);
                }

                if (var.isContainer) {
                    var.vendorExtensions.put("x-parameter-type", getTypeHint(var.dataType + "[]"));
                }

                // Create a variable to display the correct data type in comments for models
                var.vendorExtensions.put("x-comment-type", var.dataType);
                if (var.isContainer) {
                    var.vendorExtensions.put("x-comment-type", var.dataType + "[]");
                }
            }
        }

        return objs;
    }

    /**
     * Output the Getter name for boolean property, e.g. isActive
     *
     * @param name the name of the property
     * @return getter name based on naming convention
     */
    public String toBooleanGetter(String name) {
        return "is" + getterAndSetterCapitalize(name);
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiTestFileFolder() {
        return (outputFolder + File.separator + toSrcPath(apiTestsPackage, srcBasePath));
    }

    @Override
    public String modelTestFileFolder() {
        return (outputFolder + File.separator + toSrcPath(modelTestsPackage, srcBasePath));
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
        controllerTestsPackage = testsPackage + "\\" + controllerDirName;
        servicePackage = invokerPackage + "\\" + serviceDirName;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getTypeDeclaration(inner);
        }

        if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getTypeDeclaration(inner);
        }

        if (!StringUtils.isEmpty(p.get$ref())) {
            return getTypeDeclaration(getPropertyTypeDeclaration(p));
        }

        return getPropertyTypeDeclaration(p);
    }

    /**
     * Output the type declaration of the property
     *
     * @param p OpenAPI Schema object
     * @return a string presentation of the property type
     */
    public String getPropertyTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);
        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }
        return openAPIType;
    }

    @Override
    public String getTypeDeclaration(String name) {
        if (!languageSpecificPrimitives.contains(name)) {
            return modelPackage + "\\" + name;
        }
        return super.getTypeDeclaration(name);
    }

    /**
     * Return the fully-qualified "Model" name for import
     *
     * @param name the name of the "Model"
     * @return the fully-qualified "Model" name for import
     */
    @Override
    public String toModelImport(String name) {
        if ("".equals(modelPackage())) {
            return name;
        } else {
            return modelPackage() + "\\" + name;
        }
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    /**
     * Return the regular expression/JSON schema pattern (http://json-schema.org/latest/json-schema-validation.html#anchor33)
     *
     * @param pattern the pattern (regular expression)
     * @return properly-escaped pattern
     */
    @Override
    public String toRegularExpression(String pattern) {
        return escapeText(pattern);
    }

    public String toApiName(String name) {
        if (name.isEmpty()) {
            return "DefaultApiInterface";
        }
        return camelize(name, false) + "ApiInterface";
    }

    protected String toControllerName(String name) {
        if (name.isEmpty()) {
            return "DefaultController";
        }
        return camelize(name, false) + "Controller";
    }

    protected String toSymfonyService(String name) {
        String prefix = composerVendorName + ".api.";
        if (name.isEmpty()) {
            return prefix + "default";
        }

        return prefix + name;
    }

    protected String getTypeHint(String type) {
        // Type hint array types
        if (type.endsWith("[]")) {
            return "array";
        }

        // Check if the type is a native type that is type hintable in PHP
        if (typeHintable.contains(type)) {
            return type;
        }

        // Default includes are referenced by their fully-qualified class name (including namespace)
        if (defaultIncludes.contains(type)) {
            return type;
        }

        // Model classes are assumed to be imported and we reference them by their class name
        if (isModelClass(type)) {
            // This parameter is an instance of a model
            return extractSimpleName(type);
        }

        // PHP does not support type hinting for this parameter data type
        return "";
    }

    protected Boolean isModelClass(String type) {
        return Boolean.valueOf(type.contains(modelPackage()));
    }
}
