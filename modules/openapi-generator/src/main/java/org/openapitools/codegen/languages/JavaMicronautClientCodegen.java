package org.openapitools.codegen.languages;

import lombok.Getter;
import lombok.Setter;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

import java.util.Arrays;
import java.util.List;

/**
 * @deprecated WARNING! This generator is outdated. Please use the official generator for Micronaut:
 * <a href="https://github.com/micronaut-projects/micronaut-openapi">micronaut-openapi</a> with the help of the plugin for
 * <a href="https://github.com/micronaut-projects/micronaut-gradle-plugin">Gradle</a> and
 * <a href="https://github.com/micronaut-projects/micronaut-maven-plugin">Maven</a>.
 */
@SuppressWarnings("removal")
@Deprecated(forRemoval = true)
public class JavaMicronautClientCodegen extends JavaMicronautAbstractCodegen {

    public static final String OPT_CONFIGURE_AUTH = "configureAuth";
    public static final String OPT_CONFIGURE_AUTH_FILTER_PATTERN = "configureAuthFilterPattern";
    public static final String OPT_CONFIGURE_CLIENT_ID = "configureClientId";
    public static final String ADDITIONAL_CLIENT_TYPE_ANNOTATIONS = "additionalClientTypeAnnotations";
    public static final String AUTHORIZATION_FILTER_PATTERN = "authorizationFilterPattern";
    public static final String BASE_PATH_SEPARATOR = "basePathSeparator";
    public static final String CLIENT_ID = "clientId";

    public static final String NAME = "java-micronaut-client";

    @Getter protected boolean configureAuthorization;
    @Setter protected List<String> additionalClientTypeAnnotations;
    protected String authorizationFilterPattern;
    @Setter protected String basePathSeparator = "-";
    protected String clientId;

    public JavaMicronautClientCodegen() {
        super();

        title = "OpenAPI Micronaut Client";
        configureAuthorization = false;

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();
        additionalProperties.put("client", "true");

        cliOptions.add(CliOption.newBoolean(OPT_CONFIGURE_AUTH, "Configure all the authorization methods as specified in the file", configureAuthorization));
        cliOptions.add(CliOption.newString(ADDITIONAL_CLIENT_TYPE_ANNOTATIONS, "Additional annotations for client type(class level annotations). List separated by semicolon(;) or new line (Linux or Windows)"));
        cliOptions.add(CliOption.newString(AUTHORIZATION_FILTER_PATTERN, "Configure the authorization filter pattern for the client. Generally defined when generating clients from multiple specification files"));
        cliOptions.add(CliOption.newString(BASE_PATH_SEPARATOR, "Configure the separator to use between the application name and base path when referencing the property").defaultValue(basePathSeparator));
        cliOptions.add(CliOption.newString(CLIENT_ID, "Configure the service ID for the Client"));
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
        return "Generates a Java Micronaut Client. IMPORTANT: this generator has been deprecated. Please use the official one instead: https://github.com/micronaut-projects/micronaut-openapi.";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        convertPropertyToBooleanAndWriteBack(OPT_CONFIGURE_AUTH, value -> this.configureAuthorization = value);

        // Write property that is present in server
        writePropertyBack(OPT_USE_AUTH, true);

        writePropertyBack(OPT_CONFIGURE_AUTH_FILTER_PATTERN, false);
        writePropertyBack(OPT_CONFIGURE_CLIENT_ID, false);
        convertPropertyToStringAndWriteBack(BASE_PATH_SEPARATOR, this::setBasePathSeparator);

        final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");

        // Authorization files
        if (configureAuthorization) {
            final String authFolder = invokerFolder + "/auth";
            supportingFiles.add(new SupportingFile("client/auth/Authorization.mustache", authFolder, "Authorization.java"));
            supportingFiles.add(new SupportingFile("client/auth/AuthorizationBinder.mustache", authFolder, "AuthorizationBinder.java"));
            supportingFiles.add(new SupportingFile("client/auth/Authorizations.mustache", authFolder, "Authorizations.java"));
            supportingFiles.add(new SupportingFile("client/auth/AuthorizationFilter.mustache", authFolder, "AuthorizationFilter.java"));
            final String authConfigurationFolder = authFolder + "/configuration";
            supportingFiles.add(new SupportingFile("client/auth/configuration/ApiKeyAuthConfiguration.mustache", authConfigurationFolder, "ApiKeyAuthConfiguration.java"));
            supportingFiles.add(new SupportingFile("client/auth/configuration/ConfigurableAuthorization.mustache", authConfigurationFolder, "ConfigurableAuthorization.java"));
            supportingFiles.add(new SupportingFile("client/auth/configuration/HttpBasicAuthConfiguration.mustache", authConfigurationFolder, "HttpBasicAuthConfiguration.java"));

            if (additionalProperties.containsKey(AUTHORIZATION_FILTER_PATTERN)) {
                String pattern = additionalProperties.get(AUTHORIZATION_FILTER_PATTERN).toString();
                this.setAuthorizationFilterPattern(pattern);
                additionalProperties.put(AUTHORIZATION_FILTER_PATTERN, authorizationFilterPattern);
            }
        }

        if (additionalProperties.containsKey(ADDITIONAL_CLIENT_TYPE_ANNOTATIONS)) {
            String additionalClientAnnotationsList = additionalProperties.get(ADDITIONAL_CLIENT_TYPE_ANNOTATIONS).toString();
            this.setAdditionalClientTypeAnnotations(Arrays.asList(additionalClientAnnotationsList.trim().split("\\s*(;|\\r?\\n)\\s*")));
            additionalProperties.put(ADDITIONAL_CLIENT_TYPE_ANNOTATIONS, additionalClientTypeAnnotations);
        }

        if (additionalProperties.containsKey(CLIENT_ID)) {
            String id = additionalProperties.get(CLIENT_ID).toString();
            this.setClientId(id);
            additionalProperties.put(CLIENT_ID, clientId);
        }

        if (additionalProperties.containsKey(BASE_PATH_SEPARATOR)) {
            String separator = additionalProperties.get(BASE_PATH_SEPARATOR).toString();
            this.setBasePathSeparator(separator);
            additionalProperties.put(BASE_PATH_SEPARATOR, basePathSeparator);
        }

        // Api file
        apiTemplateFiles.clear();
        apiTemplateFiles.put("client/api.mustache", ".java");

        // Add test files
        apiTestTemplateFiles.clear();
        if (testTool.equals(OPT_TEST_JUNIT)) {
            apiTestTemplateFiles.put("client/test/api_test.mustache", ".java");
        } else if (testTool.equals(OPT_TEST_SPOCK)) {
            apiTestTemplateFiles.put("client/test/api_test.groovy.mustache", ".groovy");
        }

        // Add documentation files
        supportingFiles.add(new SupportingFile("client/doc/README.mustache", "", "README.md").doNotOverwrite());
        supportingFiles.add(new SupportingFile("client/doc/auth.mustache", apiDocPath, "auth.md"));
        apiDocTemplateFiles.clear();
        apiDocTemplateFiles.put("client/doc/api_doc.mustache", ".md");
    }

    public void setAuthorizationFilterPattern(final String pattern) {
        writePropertyBack(OPT_CONFIGURE_AUTH_FILTER_PATTERN, true);
        this.authorizationFilterPattern = pattern;
    }

    public void setClientId(final String id) {
        writePropertyBack(OPT_CONFIGURE_CLIENT_ID, true);
        this.clientId = id;
    }
}
