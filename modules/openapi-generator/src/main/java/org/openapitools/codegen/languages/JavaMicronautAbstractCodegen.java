package org.openapitools.codegen.languages;

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;

import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;

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

    protected String title;
    protected boolean useBeanValidation;
    protected String buildTool;
    protected String testTool;

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
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations", useBeanValidation));

        CliOption buildToolOption = new CliOption(OPT_BUILD, "Specify for which build tool to generate files").defaultValue(buildTool);
        buildToolOption.setEnum(new HashMap<String, String>() {{
            put(OPT_BUILD_GRADLE, "Gradle configuration is generated for the project");
            put(OPT_BUILD_MAVEN, "Maven configuration is generated for the project");
            put(OPT_BUILD_ALL, "Both Gradle and Maven configurations are generated");
        }});
        cliOptions.add(buildToolOption);

        CliOption testToolOption = new CliOption(OPT_TEST, "Specify which test tool to generate files for").defaultValue(testTool);
        testToolOption.setEnum(new HashMap<String, String>() {{
            put(OPT_TEST_JUNIT, "Use JUnit as test tool");
            put(OPT_TEST_SPOCK, "Use Spock as test tool");
        }});
        cliOptions.add(testToolOption);

        // Remove the date library option
        cliOptions.stream().filter(o -> o.getOpt().equals("dateLibrary")).findFirst()
                .ifPresent(v -> cliOptions.remove(v));

        // Add reserved words
        String[] reservedWordsArray = new String[]{
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

        // Get boolean properties
        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

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
            supportingFiles.add(new SupportingFile("common/configuration/mavenw/maven-wrapper.properties.mustache", ".mvn/wrapper", "maren-wrapper.properties"));
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
}
