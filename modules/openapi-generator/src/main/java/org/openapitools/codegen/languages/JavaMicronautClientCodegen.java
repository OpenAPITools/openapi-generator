package org.openapitools.codegen.languages;

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class JavaMicronautClientCodegen extends JavaMicronautAbstractCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(JavaClientCodegen.class);

    public static final String OPT_CONFIGURE_AUTH = "configureAuth";

    public static final String NAME = "java-micronaut-client";

    protected boolean configureAuthorization;

    public JavaMicronautClientCodegen() {
        super();

        title = "OpenAPI Micronaut Client";
        configureAuthorization = false;

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();
        additionalProperties.put("client", "true");

        cliOptions.add(CliOption.newBoolean(OPT_CONFIGURE_AUTH, "Configure all the authorization methods as specified in the file", configureAuthorization));
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

    public boolean isConfigureAuthorization() {
        return configureAuthorization;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(OPT_CONFIGURE_AUTH)) {
            this.configureAuthorization = convertPropertyToBoolean(OPT_CONFIGURE_AUTH);
        }
        writePropertyBack(OPT_CONFIGURE_AUTH, configureAuthorization);

        // Write property that is present in server
        writePropertyBack(OPT_USE_AUTH, true);

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
}
