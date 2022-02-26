package org.openapitools.codegen.languages;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;

import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.CodegenConstants.INVOKER_PACKAGE;

public abstract class JavaMicronautAbstractCodegen extends AbstractJavaCodegen implements BeanValidationFeatures {
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

    protected String title;
    protected boolean useBeanValidation;
    protected String buildTool;
    protected String testTool;
    protected boolean requiredPropertiesInConstructor = true;
    protected String micronautVersion = "3.3.1";

    public static final String CONTENT_TYPE_APPLICATION_FORM_URLENCODED = "application/x-www-form-urlencoded";
    public static final String CONTENT_TYPE_APPLICATION_JSON = "application/json";
    public static final String CONTENT_TYPE_MULTIPART_FORM_DATA = "multipart/form-data";
    public static final String CONTENT_TYPE_ANY = "*/*";

    public JavaMicronautAbstractCodegen() {
        super();

        // Set all the fields
        useBeanValidation = true;
        buildTool = OPT_BUILD_ALL;
        testTool = OPT_TEST_JUNIT;
        outputFolder = "generated-code/java-micronaut-client";
        templateDir = "java-micronaut/client";
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";
        invokerPackage = "org.openapitools";
        artifactId = "openapi-micronaut";
        embeddedTemplateDir = templateDir = "java-micronaut";
        apiDocPath = "docs/apis";
        modelDocPath = "docs/models";

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
        additionalProperties.put("jackson", "true");
        additionalProperties.put("openbrace", "{");
        additionalProperties.put("closebrace", "}");

        // Set client options that will be presented to user
        updateOption(INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        cliOptions.add(new CliOption(OPT_TITLE, "Client service name").defaultValue(title));
        cliOptions.add(new CliOption(OPT_MICRONAUT_VERSION, "Micronaut version, only >=3.0.0 versions are supported").defaultValue(micronautVersion));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations", useBeanValidation));
        cliOptions.add(CliOption.newBoolean(OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR, "Allow only to create models with all the required properties provided in constructor", requiredPropertiesInConstructor));

        CliOption buildToolOption = new CliOption(OPT_BUILD, "Specify for which build tool to generate files").defaultValue(buildTool);
        Map buildToolOptionMap = new HashMap<String, String>();
        buildToolOptionMap.put(OPT_BUILD_GRADLE, "Gradle configuration is generated for the project");
        buildToolOptionMap.put(OPT_BUILD_MAVEN, "Maven configuration is generated for the project");
        buildToolOptionMap.put(OPT_BUILD_ALL, "Both Gradle and Maven configurations are generated");
        buildToolOption.setEnum(buildToolOptionMap);
        cliOptions.add(buildToolOption);

        CliOption testToolOption = new CliOption(OPT_TEST, "Specify which test tool to generate files for").defaultValue(testTool);
        Map testToolOptionMap = new HashMap<String, String>();
        testToolOptionMap.put(OPT_TEST_JUNIT, "Use JUnit as test tool");
        testToolOptionMap.put(OPT_TEST_SPOCK, "Use Spock as test tool");
        testToolOption.setEnum(testToolOptionMap);
        cliOptions.add(testToolOption);

        // Remove the date library option
        cliOptions.stream().filter(o -> o.getOpt().equals("dateLibrary")).findFirst()
                .ifPresent(v -> cliOptions.remove(v));

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

        // Get boolean properties
        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        if (additionalProperties.containsKey(OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR)) {
            this.requiredPropertiesInConstructor = convertPropertyToBoolean(OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR);
        }
        writePropertyBack(OPT_REQUIRED_PROPERTIES_IN_CONSTRUCTOR, requiredPropertiesInConstructor);

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

        // Use the default java LocalDate
        typeMapping.put("date", "LocalDate");
        typeMapping.put("DateTime", "LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");

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

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        Map<String, CodegenModel> models = allModels.stream()
                .map(v -> ((Map<String, CodegenModel>) v).get("model"))
                .collect(Collectors.toMap(v -> v.classname, v -> v));
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

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
                            allowableValues, null, null, m.requiredVars, false);
                    groovyExample = getExampleValue(m.defaultValue, null, m.classname, true,
                            allowableValues, null, null, m.requiredVars, true);
                } else {
                    example = getExampleValue(null, null, op.returnType, false, null,
                            op.returnBaseType, null, null, false);
                    groovyExample = getExampleValue(null, null, op.returnType, false, null,
                            op.returnBaseType, null, null, true);
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
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        objs = super.postProcessAllModels(objs);

        for (String modelName: objs.keySet()) {
            CodegenModel model = ((Map<String, List<Map<String, CodegenModel>>>) objs.get(modelName))
                    .get("models").get(0).get("model");
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
                p.requiredVars, groovy);
    }

    protected String getPropertyExampleValue(CodegenProperty p, boolean groovy) {
        List<Object> allowableValues = p.allowableValues == null ? null : (List<Object>) p.allowableValues.get("values");

        return getExampleValue(p.defaultValue, p.example, p.dataType, p.isModel, allowableValues,
                p.items == null ? null : p.items.dataType,
                p.items == null ? null : p.items.defaultValue,
                null, groovy);
    }

    public String getExampleValue(
            String defaultValue, String example, String dataType, Boolean isModel, List<Object> allowableValues,
            String itemsType, String itemsExample, List<CodegenProperty> requiredVars, boolean groovy
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
            example = dataType + ".fromValue(\"" + value + "\")";
        } else if ((isModel != null && isModel) || (isModel == null && !languageSpecificPrimitives.contains(dataType))) {
            if (requiredVars == null) {
                example = null;
            } else {
                if (requiredPropertiesInConstructor) {
                    StringBuilder builder = new StringBuilder();
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
}
