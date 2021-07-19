package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.util.*;

import static org.openapitools.codegen.CodegenConstants.INVOKER_PACKAGE;


public class JavaMicronautClientCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures {

    private final Logger LOGGER = LoggerFactory.getLogger(JavaClientCodegen.class);

    public static final String TITLE = "title";
    public static final String CONFIG_PACKAGE = "configPackage";

    public static final String NAME = "micronaut-client";

    protected String title;
    protected String configPackage;
    protected boolean useBeanValidation;
    protected boolean performBeanValidation;

    public JavaMicronautClientCodegen() {
        super();

        title = "OpenAPI Micronaut Client";
        invokerPackage = "org.openapitools";
        configPackage = "org.openapitools.configuration";
        useBeanValidation = true;
        performBeanValidation = true;

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
                        SecurityFeature.OAuth2_Password
                ))
        );

        outputFolder = "generated-code/javaMicronaut";
        embeddedTemplateDir = templateDir = "java-micronaut-client";
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";
        invokerPackage = "org.openapitools";
        artifactId = "openapi-micronaut";

        updateOption(INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        apiTestTemplateFiles.clear();

        additionalProperties.put("jackson", "true");
        additionalProperties.put("openbrace", "{");
        additionalProperties.put("closebrace", "}");

        cliOptions.add(new CliOption(TITLE, "Client service name").defaultValue(title));
        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code").defaultValue(configPackage));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations", useBeanValidation));
        cliOptions.add(CliOption.newBoolean(PERFORM_BEANVALIDATION, "Use Bean Validation Impl. to perform BeanValidation", performBeanValidation));

        // Remove the date library option
        cliOptions.stream().filter(o -> o.getOpt().equals("dateLibrary")).findFirst()
                .ifPresent(v -> cliOptions.remove(v));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    public String getHelp() {
        return "Generates a Java Micronaut Client.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(TITLE)) {
            this.title = (String) additionalProperties.get(TITLE);
        }

        if (additionalProperties.containsKey(CONFIG_PACKAGE)) {
            configPackage = (String) additionalProperties.get(CONFIG_PACKAGE);
        } else {
            additionalProperties.put(CONFIG_PACKAGE, configPackage);
        }

        if (additionalProperties.containsKey(INVOKER_PACKAGE)) {
            invokerPackage = (String) additionalProperties.get(INVOKER_PACKAGE);
        } else {
            additionalProperties.put(INVOKER_PACKAGE, invokerPackage);
        }

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }
        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        if (additionalProperties.containsKey(PERFORM_BEANVALIDATION)) {
            this.setPerformBeanValidation(convertPropertyToBoolean(PERFORM_BEANVALIDATION));
        }
        writePropertyBack(PERFORM_BEANVALIDATION, performBeanValidation);

        final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");
        final String apiFolder = (sourceFolder + '/' + apiPackage).replace(".", "/");

        // Add all the supporting files
        String resourceFolder = projectFolder + "/resources";
        supportingFiles.add(new SupportingFile("configuration/application.yml.mustache", resourceFolder, "application.yml").doNotOverwrite());
        final String authFolder = invokerFolder + "/auth";
        supportingFiles.add(new SupportingFile("auth/Authorization.mustache", authFolder, "Authorization.java"));
        supportingFiles.add(new SupportingFile("auth/AuthorizationBinder.mustache", authFolder, "AuthorizationBinder.java"));
        supportingFiles.add(new SupportingFile("auth/Authorizations.mustache", authFolder, "Authorizations.java"));
        supportingFiles.add(new SupportingFile("auth/AuthorizationFilter.mustache", authFolder, "AuthorizationFilter.java"));
        final String authConfigurationFolder = authFolder + "/configuration";
        supportingFiles.add(new SupportingFile("auth/configuration/ApiKeyAuthConfiguration.mustache", authConfigurationFolder, "ApiKeyAuthConfiguration.java"));
        supportingFiles.add(new SupportingFile("auth/configuration/ConfigurableAuthorization.mustache", authConfigurationFolder, "ConfigurableAuthorization.java"));
        supportingFiles.add(new SupportingFile("auth/configuration/HttpBasicAuthConfiguration.mustache", authConfigurationFolder, "HttpBasicAuthConfiguration.java"));
        final String queryFolder = invokerFolder + "/query";
        supportingFiles.add(new SupportingFile("query/QueryParam.mustache", queryFolder, "QueryParam.java"));
        supportingFiles.add(new SupportingFile("query/QueryParamBinder.mustache", queryFolder, "QueryParamBinder.java"));

        supportingFiles.add(new SupportingFile("doc/README.mustache", "", "README.md").doNotOverwrite());

        // Gradle files
        supportingFiles.add(new SupportingFile("configuration/gradle/build.gradle.mustache", "", "build.gradle").doNotOverwrite());
        supportingFiles.add(new SupportingFile("configuration/gradle/settings.gradle.mustache", "", "settings.gradle").doNotOverwrite());
        supportingFiles.add(new SupportingFile("configuration/gradle/gradle.properties.mustache", "", "gradle.properties").doNotOverwrite());

        // Gradlew files
        final String gradleWrapperFolder = "gradle/wrapper";
        supportingFiles.add(new SupportingFile("configuration/gradlew/gradlew.mustache", "", "gradlew"));
        supportingFiles.add(new SupportingFile("configuration/gradlew/gradlew.bat.mustache", "", "gradlew.bat"));
        supportingFiles.add(new SupportingFile("configuration/gradlew/gradle-wrapper.properties.mustache", gradleWrapperFolder, "gradle-wrapper.properties"));
        supportingFiles.add(new SupportingFile("configuration/gradlew/gradle-wrapper.jar", gradleWrapperFolder, "gradle-wrapper.jar"));

        // Git files
        supportingFiles.add(new SupportingFile("configuration/git/gitignore.mustache", "", ".gitignore").doNotOverwrite());

        // Use the default java Date
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "Date");
        importMapping.put("Date", "java.util.Date");

        // Add documentation files
        modelDocTemplateFiles.put("doc/model_doc.mustache", ".md");
        apiDocTemplateFiles.put("doc/api_doc.mustache", ".md");
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        // Add model files
        modelTemplateFiles.remove("model.mustache");
        modelTemplateFiles.put("model/model.mustache", ".java");

        // Add test files
        apiTestTemplateFiles.put("api_test.mustache", ".java");
        modelTestTemplateFiles.put("model_test.mustache", ".java");
    }


    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    @Override
    public void setPerformBeanValidation(boolean performBeanValidation) {
        this.performBeanValidation = performBeanValidation;
    }
}
