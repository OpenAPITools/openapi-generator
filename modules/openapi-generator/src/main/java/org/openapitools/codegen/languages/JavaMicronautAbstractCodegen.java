package org.openapitools.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.OptionalFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.Writer;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.CodegenConstants.INVOKER_PACKAGE;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public abstract class JavaMicronautAbstractCodegen extends AbstractJavaCodegen implements BeanValidationFeatures, OptionalFeatures {
    public static final String OPT_TITLE = "title";
    public static final String OPT_BUILD = "build";
    public static final String OPT_BUILD_GRADLE = "gradle";
    public static final String OPT_BUILD_MAVEN = "maven";
    public static final String OPT_BUILD_ALL = "all";
    public static final String OPT_TEST = "test";
    public static final String OPT_TEST_JUNIT = "junit";
    public static final String OPT_TEST_SPOCK = "spock";
    public static final String OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR = "requiredPropertiesInConstructor";
    public static final String OPT_MICRONAUT_VERSION = "micronautVersion";
    public static final String OPT_USE_AUTH = "useAuth";
    public static final String OPT_VISITABLE = "visitable";
    public static final String OPT_DATE_LIBRARY_JAVA8 = "java8";
    public static final String OPT_DATE_LIBRARY_JAVA8_LOCAL_DATETIME = "java8-localdatetime";
    public static final String OPT_DATE_FORMAT = "dateFormat";
    public static final String OPT_DATETIME_FORMAT = "datetimeFormat";
    public static final String OPT_REACTIVE = "reactive";
    public static final String OPT_WRAP_IN_HTTP_RESPONSE = "wrapInHttpResponse";
    public static final String OPT_APPLICATION_NAME = "applicationName";
    public static final String OPT_GENERATE_SWAGGER_ANNOTATIONS = "generateSwaggerAnnotations";
    public static final String OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_1 = "swagger1";
    public static final String OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_2 = "swagger2";
    public static final String OPT_GENERATE_SWAGGER_ANNOTATIONS_TRUE = "true";
    public static final String OPT_GENERATE_SWAGGER_ANNOTATIONS_FALSE = "false";
    public static final String OPT_GENERATE_OPERATION_ONLY_FOR_FIRST_TAG = "generateOperationOnlyForFirstTag";
    public enum SERIALIZATION_LIBRARY_TYPE {jackson, micronaut_serde_jackson}

    protected final Logger LOGGER = LoggerFactory.getLogger(JavaMicronautAbstractCodegen.class);

    protected String title;
    protected boolean useBeanValidation;
    protected boolean useOptional;
    protected boolean visitable;
    protected String buildTool;
    protected String testTool;
    protected boolean requiredPropertiesInConstructor = true;
    protected String micronautVersion;
    protected boolean reactive;
    protected boolean wrapInHttpResponse;
    protected String appName;
    protected String generateSwaggerAnnotations;
    protected boolean generateOperationOnlyForFirstTag;
    protected String serializationLibrary = SERIALIZATION_LIBRARY_TYPE.jackson.name();

    public static final String CONTENT_TYPE_APPLICATION_FORM_URLENCODED = "application/x-www-form-urlencoded";
    public static final String CONTENT_TYPE_APPLICATION_JSON = "application/json";
    public static final String CONTENT_TYPE_MULTIPART_FORM_DATA = "multipart/form-data";
    public static final String CONTENT_TYPE_ANY = "*/*";
    public static final String DATE_FORMAT = "yyyy-MM-dd";
    public static final String DATETIME_FORMAT = DATE_FORMAT + "'T'HH:mm:ss.SSS";
    public static final String OFFSET_DATETIME_FORMAT = DATETIME_FORMAT + "XXXX";

    public JavaMicronautAbstractCodegen() {
        super();

        // Set all the fields
        useBeanValidation = true;
        useOptional = false;
        visitable = false;
        buildTool = OPT_BUILD_ALL;
        testTool = OPT_TEST_JUNIT;
        outputFolder = this instanceof JavaMicronautClientCodegen ?
                "generated-code/java-micronaut-client" : "generated-code/java-micronaut";
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";
        invokerPackage = "org.openapitools";
        artifactId = this instanceof JavaMicronautClientCodegen ?
                "openapi-micronaut-client" : "openapi-micronaut";
        embeddedTemplateDir = templateDir = "java-micronaut";
        apiDocPath = "docs/apis";
        modelDocPath = "docs/models";
        dateLibrary = OPT_DATE_LIBRARY_JAVA8;
        micronautVersion = "3.4.3";
        reactive = true;
        wrapInHttpResponse = false;
        appName = artifactId;
        generateSwaggerAnnotations = this instanceof JavaMicronautClientCodegen ?
                OPT_GENERATE_SWAGGER_ANNOTATIONS_FALSE : OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_2;
        generateOperationOnlyForFirstTag = this instanceof JavaMicronautServerCodegen;

        // Set implemented features for user information
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(
                        DocumentationFeature.Readme
                )
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_ClientCredentials,
                        SecurityFeature.OAuth2_Password,
                        SecurityFeature.OpenIDConnect
                ))
        );

        // Set additional properties
        additionalProperties.put("openbrace", "{");
        additionalProperties.put("closebrace", "}");

        // Set client options that will be presented to user
        updateOption(INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        cliOptions.add(new CliOption(OPT_TITLE, "Client service name").defaultValue(title));
        cliOptions.add(new CliOption(OPT_MICRONAUT_VERSION, "Micronaut version, only >=3.0.0 versions are supported").defaultValue(micronautVersion));
        cliOptions.add(new CliOption(OPT_APPLICATION_NAME, "Micronaut application name (Defaults to the " + CodegenConstants.ARTIFACT_ID + " value)").defaultValue(appName));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations", useBeanValidation));
        cliOptions.add(CliOption.newBoolean(USE_OPTIONAL, "Use Optional container for optional parameters", useOptional));
        cliOptions.add(CliOption.newBoolean(OPT_VISITABLE, "Generate visitor for subtypes with a discriminator", visitable));
        cliOptions.add(CliOption.newBoolean(OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR, "Allow only to create models with all the required properties provided in constructor", requiredPropertiesInConstructor));
        cliOptions.add(CliOption.newBoolean(OPT_REACTIVE, "Make the responses use Reactor Mono as wrapper", reactive));
        cliOptions.add(CliOption.newBoolean(OPT_WRAP_IN_HTTP_RESPONSE, "Wrap the response in HttpResponse object", wrapInHttpResponse));
        cliOptions.add(CliOption.newBoolean(OPT_GENERATE_OPERATION_ONLY_FOR_FIRST_TAG, "When false, the operation method will be duplicated in each of the tags if multiple tags are assigned to this operation. " +
                "If true, each operation will be generated only once in the first assigned tag.", generateOperationOnlyForFirstTag));

        CliOption buildToolOption = new CliOption(OPT_BUILD, "Specify for which build tool to generate files").defaultValue(buildTool);
        Map<String, String> buildToolOptionMap = new HashMap<>();
        buildToolOptionMap.put(OPT_BUILD_GRADLE, "Gradle configuration is generated for the project");
        buildToolOptionMap.put(OPT_BUILD_MAVEN, "Maven configuration is generated for the project");
        buildToolOptionMap.put(OPT_BUILD_ALL, "Both Gradle and Maven configurations are generated");
        buildToolOption.setEnum(buildToolOptionMap);
        cliOptions.add(buildToolOption);

        CliOption testToolOption = new CliOption(OPT_TEST, "Specify which test tool to generate files for").defaultValue(testTool);
        Map<String, String> testToolOptionMap = new HashMap<>();
        testToolOptionMap.put(OPT_TEST_JUNIT, "Use JUnit as test tool");
        testToolOptionMap.put(OPT_TEST_SPOCK, "Use Spock as test tool");
        testToolOption.setEnum(testToolOptionMap);
        cliOptions.add(testToolOption);

        CliOption generateSwaggerAnnotationsOption = new CliOption(OPT_GENERATE_SWAGGER_ANNOTATIONS, "Specify if you want to generate swagger annotations and which version").defaultValue(generateSwaggerAnnotations);
        Map<String, String> generateSwaggerAnnotationsOptionMap = new HashMap<>();
        generateSwaggerAnnotationsOptionMap.put(OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_1, "Use io.swagger:swagger-annotations for annotating operations and schemas");
        generateSwaggerAnnotationsOptionMap.put(OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_2, "Use io.swagger.core.v3:swagger-annotations for annotating operations and schemas");
        generateSwaggerAnnotationsOptionMap.put(OPT_GENERATE_SWAGGER_ANNOTATIONS_TRUE, "Equivalent to \"" + OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_2 + "\"");
        generateSwaggerAnnotationsOptionMap.put(OPT_GENERATE_SWAGGER_ANNOTATIONS_FALSE, "Do not generate swagger annotations");
        generateSwaggerAnnotationsOption.setEnum(generateSwaggerAnnotationsOptionMap);
        cliOptions.add(generateSwaggerAnnotationsOption);

        cliOptions.add(new CliOption(OPT_DATE_FORMAT, "Specify the format pattern of date as a string"));
        cliOptions.add(new CliOption(OPT_DATETIME_FORMAT, "Specify the format pattern of date-time as a string"));

        // Modify the DATE_LIBRARY option to only have supported values
        cliOptions.stream().filter(o -> o.getOpt().equals(DATE_LIBRARY)).findFirst().ifPresent(opt -> {
            Map<String, String> valuesEnum = new HashMap<>();
            valuesEnum.put(OPT_DATE_LIBRARY_JAVA8, opt.getEnum().get(OPT_DATE_LIBRARY_JAVA8));
            valuesEnum.put(OPT_DATE_LIBRARY_JAVA8_LOCAL_DATETIME, opt.getEnum().get(OPT_DATE_LIBRARY_JAVA8_LOCAL_DATETIME));
            opt.setEnum(valuesEnum);
        });

        final CliOption serializationLibraryOpt = CliOption.newString(CodegenConstants.SERIALIZATION_LIBRARY, "Serialization library for model");
        serializationLibraryOpt.defaultValue(SERIALIZATION_LIBRARY_TYPE.jackson.name());
        Map<String, String> serializationLibraryOptions = new HashMap<>();
        serializationLibraryOptions.put(SERIALIZATION_LIBRARY_TYPE.jackson.name(), "Jackson as serialization library");
        serializationLibraryOptions.put(SERIALIZATION_LIBRARY_TYPE.micronaut_serde_jackson.name(), "Use micronaut-serialization with Jackson annotations");
        serializationLibraryOpt.setEnum(serializationLibraryOptions);
        cliOptions.add(serializationLibraryOpt);

        // Add reserved words
        String[] reservedWordsArray = {
                "client", "format", "queryvalue", "queryparam", "pathvariable", "header", "cookie",
                "authorization", "body", "application"
        };
        reservedWords.addAll(Arrays.asList(reservedWordsArray));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Get properties
        if (additionalProperties.containsKey(OPT_TITLE)) {
            this.title = (String) additionalProperties.get(OPT_TITLE);
        }

        if (additionalProperties.containsKey(INVOKER_PACKAGE)) {
            invokerPackage = (String) additionalProperties.get(INVOKER_PACKAGE);
        } else {
            additionalProperties.put(INVOKER_PACKAGE, invokerPackage);
        }

        if (additionalProperties.containsKey(OPT_MICRONAUT_VERSION)) {
            micronautVersion = (String) additionalProperties.get(OPT_MICRONAUT_VERSION);
        } else {
            additionalProperties.put(OPT_MICRONAUT_VERSION, micronautVersion);
        }

        if (additionalProperties.containsKey(OPT_APPLICATION_NAME)) {
            appName = (String) additionalProperties.get(OPT_APPLICATION_NAME);
        } else {
            additionalProperties.put(OPT_APPLICATION_NAME, artifactId);
        }

        // Get boolean properties
        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        if (additionalProperties.containsKey(USE_OPTIONAL)) {
            this.setUseOptional(convertPropertyToBoolean(USE_OPTIONAL));
        }
        writePropertyBack(USE_OPTIONAL, useOptional);

        if (additionalProperties.containsKey(OPT_VISITABLE)) {
            this.setVisitable(convertPropertyToBoolean(OPT_VISITABLE));
        }
        writePropertyBack(OPT_VISITABLE, visitable);

        if (additionalProperties.containsKey(OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR)) {
            this.requiredPropertiesInConstructor = convertPropertyToBoolean(OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR);
        }
        writePropertyBack(OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR, requiredPropertiesInConstructor);

        if (additionalProperties.containsKey(OPT_REACTIVE)) {
            this.reactive = convertPropertyToBoolean(OPT_REACTIVE);
        }
        writePropertyBack(OPT_REACTIVE, reactive);

        if (additionalProperties.containsKey(OPT_WRAP_IN_HTTP_RESPONSE)) {
            this.wrapInHttpResponse = convertPropertyToBoolean(OPT_WRAP_IN_HTTP_RESPONSE);
        }
        writePropertyBack(OPT_WRAP_IN_HTTP_RESPONSE, wrapInHttpResponse);

        if (additionalProperties.containsKey(OPT_GENERATE_OPERATION_ONLY_FOR_FIRST_TAG)) {
            this.generateOperationOnlyForFirstTag = convertPropertyToBoolean(OPT_GENERATE_OPERATION_ONLY_FOR_FIRST_TAG);
        }
        writePropertyBack(OPT_GENERATE_OPERATION_ONLY_FOR_FIRST_TAG, generateOperationOnlyForFirstTag);

        // Get enum properties
        if (additionalProperties.containsKey(OPT_BUILD)) {
            switch ((String) additionalProperties.get(OPT_BUILD)) {
                case OPT_BUILD_GRADLE:
                case OPT_BUILD_MAVEN:
                case OPT_BUILD_ALL:
                    this.buildTool = (String) additionalProperties.get(OPT_BUILD);
                    break;
                default:
                    throw new RuntimeException("Build tool \"" + additionalProperties.get(OPT_BUILD) + "\" is not supported or misspelled.");
            }
        }
        additionalProperties.put(OPT_BUILD, buildTool);

        if (additionalProperties.containsKey(OPT_TEST)) {
            switch ((String) additionalProperties.get(OPT_TEST)) {
                case OPT_TEST_JUNIT:
                case OPT_TEST_SPOCK:
                    this.testTool = (String) additionalProperties.get(OPT_TEST);
                    break;
                default:
                    throw new RuntimeException("Test tool \"" + additionalProperties.get(OPT_TEST) + "\" is not supported or misspelled.");
            }
        }
        additionalProperties.put(OPT_TEST, testTool);
        if (testTool.equals(OPT_TEST_JUNIT)) {
            additionalProperties.put("isTestJunit", true);
        } else if (testTool.equals(OPT_TEST_SPOCK)) {
            additionalProperties.put("isTestSpock", true);
        }

        if (additionalProperties.containsKey(OPT_GENERATE_SWAGGER_ANNOTATIONS)) {
            String value = String.valueOf(additionalProperties.get(OPT_GENERATE_SWAGGER_ANNOTATIONS));
            switch (value) {
                case OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_1:
                    this.generateSwaggerAnnotations = OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_1;
                    break;
                case OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_2:
                case OPT_GENERATE_SWAGGER_ANNOTATIONS_TRUE:
                    this.generateSwaggerAnnotations = OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_2;
                    break;
                case OPT_GENERATE_SWAGGER_ANNOTATIONS_FALSE:
                    this.generateSwaggerAnnotations = OPT_GENERATE_SWAGGER_ANNOTATIONS_FALSE;
                    break;
                default:
                    throw new RuntimeException("Value \"" + value + "\" for the " + OPT_GENERATE_SWAGGER_ANNOTATIONS + " parameter is unsupported or misspelled");
            }
        }
        if (OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_1.equals(this.generateSwaggerAnnotations)) {
            additionalProperties.put("generateSwagger1Annotations", true);
        } else if (OPT_GENERATE_SWAGGER_ANNOTATIONS_SWAGGER_2.equals(this.generateSwaggerAnnotations)) {
            additionalProperties.put("generateSwagger2Annotations", true);
        }

        if (additionalProperties.containsKey(CodegenConstants.SERIALIZATION_LIBRARY)) {
            setSerializationLibrary((String) additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY));
        }
        additionalProperties.put(this.serializationLibrary, true);

        // Add all the supporting files
        String resourceFolder = projectFolder + "/resources";
        supportingFiles.add(new SupportingFile("common/configuration/application.yml.mustache", resourceFolder, "application.yml").doNotOverwrite());
        supportingFiles.add(new SupportingFile("common/configuration/logback.xml.mustache", resourceFolder, "logback.xml").doNotOverwrite());

        if (buildTool.equals(OPT_BUILD_GRADLE) || buildTool.equals(OPT_BUILD_ALL)) {
            // Gradle files
            supportingFiles.add(new SupportingFile("common/configuration/gradle/build.gradle.mustache", "", "build.gradle").doNotOverwrite());
            supportingFiles.add(new SupportingFile("common/configuration/gradle/settings.gradle.mustache", "", "settings.gradle").doNotOverwrite());
            supportingFiles.add(new SupportingFile("common/configuration/gradle/gradle.properties.mustache", "", "gradle.properties").doNotOverwrite());

            // Gradlew files
            final String gradleWrapperFolder = "gradle/wrapper";
            supportingFiles.add(new SupportingFile("common/configuration/gradlew/gradlew.mustache", "", "gradlew"));
            supportingFiles.add(new SupportingFile("common/configuration/gradlew/gradlew.bat.mustache", "", "gradlew.bat"));
            supportingFiles.add(new SupportingFile("common/configuration/gradlew/gradle-wrapper.properties.mustache", gradleWrapperFolder, "gradle-wrapper.properties"));
            supportingFiles.add(new SupportingFile("common/configuration/gradlew/gradle-wrapper.jar", gradleWrapperFolder, "gradle-wrapper.jar"));
        }

        if (buildTool.equals(OPT_BUILD_MAVEN) || buildTool.equals(OPT_BUILD_ALL)) {
            // Maven files
            supportingFiles.add(new SupportingFile("common/configuration/pom.xml.mustache", "", "pom.xml").doNotOverwrite());

            // Maven wrapper files
            supportingFiles.add(new SupportingFile("common/configuration/mavenw/mvnw.mustache", "", "mvnw"));
            supportingFiles.add(new SupportingFile("common/configuration/mavenw/mvnw.bat.mustache", "", "mvnw.bat"));
            supportingFiles.add(new SupportingFile("common/configuration/mavenw/MavenWrapperDownloader.java.mustache", ".mvn/wrapper", "MavenWrapperDownloader.java"));
            supportingFiles.add(new SupportingFile("common/configuration/mavenw/maven-wrapper.jar.mustache", ".mvn/wrapper", "maven-wrapper.jar"));
            supportingFiles.add(new SupportingFile("common/configuration/mavenw/maven-wrapper.properties.mustache", ".mvn/wrapper", "maven-wrapper.properties"));
        }

        // Git files
        supportingFiles.add(new SupportingFile("common/configuration/git/gitignore.mustache", "", ".gitignore").doNotOverwrite());

        // Use the default java time
        additionalProperties.putIfAbsent(OPT_DATE_FORMAT, DATE_FORMAT);
        if (dateLibrary.equals(OPT_DATE_LIBRARY_JAVA8)) {
            typeMapping.put("DateTime", "OffsetDateTime");
            typeMapping.put("date", "LocalDate");
            additionalProperties.putIfAbsent(OPT_DATETIME_FORMAT, OFFSET_DATETIME_FORMAT);
        } else if (dateLibrary.equals(OPT_DATE_LIBRARY_JAVA8_LOCAL_DATETIME)) {
            typeMapping.put("DateTime", "LocalDateTime");
            typeMapping.put("date", "LocalDate");
            additionalProperties.putIfAbsent(OPT_DATETIME_FORMAT, DATETIME_FORMAT);
        }
        importMapping.putIfAbsent("LocalDateTime", "java.time.LocalDateTime");
        importMapping.putIfAbsent("OffsetDateTime", "java.time.OffsetDateTime");
        importMapping.putIfAbsent("LocalDate", "java.time.LocalDate");

        // Add documentation files
        modelDocTemplateFiles.clear();
        modelDocTemplateFiles.put("common/doc/model_doc.mustache", ".md");

        // Add model files
        modelTemplateFiles.clear();
        modelTemplateFiles.put("common/model/model.mustache", ".java");

        // Add test files
        modelTestTemplateFiles.clear();
        if (testTool.equals(OPT_TEST_JUNIT)) {
            modelTestTemplateFiles.put("common/test/model_test.mustache", ".java");
        } else if (testTool.equals(OPT_TEST_SPOCK)) {
            modelTestTemplateFiles.put("common/test/model_test.groovy.mustache", ".groovy");
        }

        // Set properties for documentation
        final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");
        final String apiFolder = (sourceFolder + '/' + apiPackage()).replace('.', '/');
        final String modelFolder = (sourceFolder + '/' + modelPackage()).replace('.', '/');
        additionalProperties.put("invokerFolder", invokerFolder);
        additionalProperties.put("resourceFolder", resourceFolder);
        additionalProperties.put("apiFolder", apiFolder);
        additionalProperties.put("modelFolder", modelFolder);
    }

    @Override
    public String apiTestFileFolder() {
        if (testTool.equals(OPT_TEST_SPOCK)) {
            return getOutputDir() + "/src/test/groovy/" + apiPackage().replaceAll("\\.", "/");
        }
        return getOutputDir() + "/src/test/java/" + apiPackage().replaceAll("\\.", "/");
    }

    @Override
    public String modelTestFileFolder() {
        if (testTool.equals(OPT_TEST_SPOCK)) {
            return getOutputDir() + "/src/test/groovy/" + modelPackage().replaceAll("\\.", "/");
        }
        return getOutputDir() + "/src/test/java/" + modelPackage().replaceAll("\\.", "/");
    }

    @Override
    public String toApiTestFilename(String name) {
        if (testTool.equals(OPT_TEST_SPOCK)) {
            return toApiName(name) + "Spec";
        }
        return toApiName(name) + "Test";
    }

    @Override
    public String toModelTestFilename(String name) {
        if (testTool.equals(OPT_TEST_SPOCK)) {
            return toModelName(name) + "Spec";
        }
        return toModelName(name) + "Test";
    }

    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    @Override
    public void setUseOptional(boolean useOptional) {
        this.useOptional = useOptional;
    }

    public void setVisitable(boolean visitable) {
        this.visitable = visitable;
    }

    @Override
    public String toApiVarName(String name) {
        String apiVarName = super.toApiVarName(name);
        if (reservedWords.contains(apiVarName)) {
            apiVarName = escapeReservedWord(apiVarName);
        }
        return apiVarName;
    }

    public boolean isUseBeanValidation() {
        return useBeanValidation;
    }

    public boolean isUseOptional() {
        return useOptional;
    }

    public boolean isVisitable() {
        return visitable;
    }

    @Override
    public String sanitizeTag(String tag) {
        // Skip sanitization to get the original tag name in the addOperationToGroup() method.
        // Inside that method tag is manually sanitized.
        return tag;
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation
            co, Map<String, List<CodegenOperation>> operations) {
        if (generateOperationOnlyForFirstTag && !co.tags.get(0).getName().equals(tag)) {
            // This is not the first assigned to this operation tag;
            return;
        }

        super.addOperationToGroup(super.sanitizeTag(tag), resourcePath, operation, co, operations);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        Map<String, CodegenModel> models = allModels.stream()
                .map(ModelMap::getModel)
                .collect(Collectors.toMap(v -> v.classname, v -> v));
        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();

        for (CodegenOperation op : operationList) {
            // Set whether body is supported in request
            op.vendorExtensions.put("methodAllowsBody",
                    op.httpMethod.equals("PUT") || op.httpMethod.equals("POST") || op.httpMethod.equals("PATCH"));

            // Set response example
            if (op.returnType != null) {
                String example;
                String groovyExample;
                if (models.containsKey(op.returnType)) {
                    CodegenModel m = models.get(op.returnType);
                    List<Object> allowableValues = null;
                    if (m.allowableValues != null && m.allowableValues.containsKey("values")) {
                        allowableValues = (List<Object>) m.allowableValues.get("values");
                    }
                    example = getExampleValue(m.defaultValue, null, m.classname, true,
                            allowableValues, null, null, m.requiredVars, false, false);
                    groovyExample = getExampleValue(m.defaultValue, null, m.classname, true,
                            allowableValues, null, null, m.requiredVars, true, false);
                } else {
                    example = getExampleValue(null, null, op.returnType, false, null,
                            op.returnBaseType, null, null, false, false);
                    groovyExample = getExampleValue(null, null, op.returnType, false, null,
                            op.returnBaseType, null, null, true, false);
                }
                op.vendorExtensions.put("example", example);
                op.vendorExtensions.put("groovyExample", groovyExample);
            }

            // Remove the "*/*" contentType from operations as it is ambiguous
            if (CONTENT_TYPE_ANY.equals(op.vendorExtensions.get("x-contentType"))) {
                op.vendorExtensions.put("x-contentType", CONTENT_TYPE_APPLICATION_JSON);
            }
            op.consumes = op.consumes == null ? null : op.consumes.stream()
                    .filter(contentType -> !CONTENT_TYPE_ANY.equals(contentType.get("mediaType")))
                    .collect(Collectors.toList());
            op.produces = op.produces == null ? null : op.produces.stream()
                    .filter(contentType -> !CONTENT_TYPE_ANY.equals(contentType.get("mediaType")))
                    .collect(Collectors.toList());

            // Force form parameters are only set if the content-type is according
            // formParams correspond to urlencoded type
            // bodyParams correspond to multipart body
            if (CONTENT_TYPE_APPLICATION_FORM_URLENCODED.equals(op.vendorExtensions.get("x-contentType"))) {
                op.formParams.addAll(op.bodyParams);
                op.bodyParams.forEach(p -> {
                    p.isBodyParam = false;
                    p.isFormParam = true;
                });
                op.bodyParams.clear();
            } else if (CONTENT_TYPE_MULTIPART_FORM_DATA.equals(op.vendorExtensions.get("x-contentType"))) {
                op.bodyParams.addAll(op.formParams);
                op.formParams.forEach(p -> {
                    p.isBodyParam = true;
                    p.isFormParam = false;
                });
                op.formParams.clear();
            }
        }

        return objs;
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        codegenModel.imports.remove("ApiModel");
        codegenModel.imports.remove("ApiModelProperty");
        return codegenModel;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);

        for (ModelsMap models: objs.values()) {
            CodegenModel model = models.getModels().get(0).getModel();
            if (model.getParentModel() != null) {
                model.vendorExtensions.put("requiredParentVars", model.getParentModel().requiredVars);
            }

            List<CodegenProperty> requiredVars = model.vars.stream().filter(v -> v.required).collect(Collectors.toList());
            model.vendorExtensions.put("requiredVars", requiredVars);
        }

        return objs;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        p.vendorExtensions.put("groovyExample", getParameterExampleValue(p, true));
        p.example = getParameterExampleValue(p, false);
    }

    protected String getParameterExampleValue(CodegenParameter p, boolean groovy) {
        List<Object> allowableValues = p.allowableValues == null ? null : (List<Object>) p.allowableValues.get("values");

        return getExampleValue(p.defaultValue, p.example, p.dataType, p.isModel, allowableValues,
                p.items == null ? null : p.items.dataType,
                p.items == null ? null : p.items.defaultValue,
                p.requiredVars, groovy, false);
    }

    protected String getPropertyExampleValue(CodegenProperty p, boolean groovy) {
        List<Object> allowableValues = p.allowableValues == null ? null : (List<Object>) p.allowableValues.get("values");

        return getExampleValue(p.defaultValue, p.example, p.dataType, p.isModel, allowableValues,
                p.items == null ? null : p.items.dataType,
                p.items == null ? null : p.items.defaultValue,
                null, groovy, true);
    }

    public String getExampleValue(
            String defaultValue, String example, String dataType, Boolean isModel, List<Object> allowableValues,
            String itemsType, String itemsExample, List<CodegenProperty> requiredVars, boolean groovy, boolean isProperty
    ) {
        example = defaultValue != null ? defaultValue : example;
        String containerType = dataType == null ? null : dataType.split("<")[0];

        if ("String".equals(dataType)) {
            if (groovy) {
                example = example != null ? "'" + escapeTextGroovy(example) + "'" : "'example'";
            } else {
                example = example != null ? "\"" + escapeText(example) + "\"" : "\"example\"";
            }
        } else if ("Integer".equals(dataType) || "Short".equals(dataType)) {
            example = example != null ? example : "56";
        } else if ("Long".equals(dataType)) {
            example = StringUtils.appendIfMissingIgnoreCase(example != null ? example : "56", "L");
        } else if ("Float".equals(dataType)) {
            example = StringUtils.appendIfMissingIgnoreCase(example != null ? example : "3.4", "F");
        } else if ("Double".equals(dataType)) {
            example = StringUtils.appendIfMissingIgnoreCase(example != null ? example : "3.4", "D");
        } else if ("Boolean".equals(dataType)) {
            example = example != null ? example : "false";
        } else if ("File".equals(dataType)) {
            example = null;
        } else if ("OffsetDateTime".equals(dataType)) {
            example = "OffsetDateTime.of(2001, 2, 3, 12, 0, 0, 0, java.time.ZoneOffset.of(\"+02:00\"))";
        } else if ("LocalDate".equals(dataType)) {
            example = "LocalDate.of(2001, 2, 3)";
        } else if ("LocalDateTime".equals(dataType)) {
            example = "LocalDateTime.of(2001, 2, 3, 4, 5)";
        } else if ("BigDecimal".equals(dataType)) {
            example = "new BigDecimal(78)";
        } else if (allowableValues != null && !allowableValues.isEmpty()) {
            // This is an enum
            Object value = example;
            if (value == null || !allowableValues.contains(value)) {
                value = allowableValues.get(0);
            }
            if (isProperty) {
                dataType = importMapping.getOrDefault(dataType, modelPackage + '.' + dataType);
            }
            example = dataType + ".fromValue(\"" + value + "\")";
        } else if ((isModel != null && isModel) || (isModel == null && !languageSpecificPrimitives.contains(dataType))) {
            if (requiredVars == null) {
                example = null;
            } else {
                if (requiredPropertiesInConstructor) {
                    StringBuilder builder = new StringBuilder();
                    if (isProperty) {
                        dataType =  importMapping.getOrDefault(dataType, modelPackage + '.' + dataType);
                    }
                    builder.append("new ").append(dataType).append("(");
                    for (int i = 0; i < requiredVars.size(); ++i) {
                        if (i != 0) {
                            builder.append(", ");
                        }
                        builder.append(getPropertyExampleValue(requiredVars.get(i), groovy));
                    }
                    builder.append(")");
                    example = builder.toString();
                } else {
                    example = "new " + dataType + "()";
                }
            }
        }

        if ("List".equals(containerType)) {
            String innerExample;
            if ("String".equals(itemsType)) {
                itemsExample = itemsExample != null ? itemsExample : "example";
                if (groovy) {
                    innerExample = "'" + escapeTextGroovy(itemsExample) + "'";
                } else {
                    innerExample = "\"" + escapeText(itemsExample) + "\"";
                }
            } else {
                innerExample = itemsExample != null ? itemsExample : "";
            }

            if (groovy) {
                example = "[" + innerExample + "]";
            } else {
                example = "Arrays.asList(" + innerExample + ")";
            }
        } else if ("Set".equals(containerType)) {
            if (groovy) {
                example = "[].asSet()";
            } else {
                example = "new HashSet<>()";
            }
        } else if ("Map".equals(containerType)) {
            if (groovy) {
                example = "[:]";
            } else {
                example = "new HashMap<>()";
            }
        } else if (example == null) {
            example = "null";
        }

        return example;
    }

    public String escapeTextGroovy(String text) {
        if (text == null) {
            return null;
        }
        return escapeText(text).replaceAll("'", "\\'");
    }

    @Override
    protected ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
            .put("replaceDotsWithUnderscore", new ReplaceDotsWithUnderscoreLambda());
    }

    private static class ReplaceDotsWithUnderscoreLambda implements Mustache.Lambda {
        @Override
        public void execute(final Template.Fragment fragment, final Writer writer) throws IOException {
            writer.write(fragment.execute().replace('.', '_'));
        }
    }

    public void setSerializationLibrary(final String serializationLibrary) {
        try {
            this.serializationLibrary = JavaMicronautAbstractCodegen.SERIALIZATION_LIBRARY_TYPE.valueOf(serializationLibrary).name();
        } catch (IllegalArgumentException ex) {
            StringBuilder sb = new StringBuilder(serializationLibrary + " is an invalid enum property naming option. Please choose from:");
            for (JavaMicronautAbstractCodegen.SERIALIZATION_LIBRARY_TYPE availableSerializationLibrary : JavaMicronautAbstractCodegen.SERIALIZATION_LIBRARY_TYPE.values()) {
                sb.append("\n  ").append(availableSerializationLibrary.name());
            }
            throw new RuntimeException(sb.toString());
        }
    }
}
