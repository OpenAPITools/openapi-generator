package io.swagger.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.mustache.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class KotlinServerCodegen extends AbstractKotlinCodegen {

    public static final String DEFAULT_LIBRARY = Constants.KTOR;
    static Logger LOGGER = LoggerFactory.getLogger(KotlinServerCodegen.class);
    private Boolean autoHeadFeatureEnabled = true;
    private Boolean conditionalHeadersFeatureEnabled = false;
    private Boolean hstsFeatureEnabled = true;
    private Boolean corsFeatureEnabled = false;
    private Boolean compressionFeatureEnabled = true;

    // This is here to potentially warn the user when an option is not supoprted by the target framework.
    private Map<String, List<String>> optionsSupportedPerFramework = new ImmutableMap.Builder<String, List<String>>()
            .put(Constants.KTOR, Arrays.asList(
                    Constants.AUTOMATIC_HEAD_REQUESTS,
                    Constants.CONDITIONAL_HEADERS,
                    Constants.HSTS,
                    Constants.CORS,
                    Constants.COMPRESSION
            ))
            .build();

    /**
     * Constructs an instance of `KotlinServerCodegen`.
     */
    public KotlinServerCodegen() {
        super();

        artifactId = "kotlin-server";
        packageName = "io.swagger.server";
        outputFolder = "generated-code" + File.separator + "kotlin-server";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        embeddedTemplateDir = templateDir = "kotlin-server";
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        supportedLibraries.put("ktor", "ktor framework");

        // TODO: Configurable server engine. Defaults to netty in build.gradle.
        CliOption library = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        library.setDefault(DEFAULT_LIBRARY);
        library.setEnum(supportedLibraries);

        cliOptions.add(library);

        addSwitch(Constants.AUTOMATIC_HEAD_REQUESTS, Constants.AUTOMATIC_HEAD_REQUESTS_DESC, getAutoHeadFeatureEnabled());
        addSwitch(Constants.CONDITIONAL_HEADERS, Constants.CONDITIONAL_HEADERS_DESC, getConditionalHeadersFeatureEnabled());
        addSwitch(Constants.HSTS, Constants.HSTS_DESC, getHstsFeatureEnabled());
        addSwitch(Constants.CORS, Constants.CORS_DESC, getCorsFeatureEnabled());
        addSwitch(Constants.COMPRESSION, Constants.COMPRESSION_DESC, getCompressionFeatureEnabled());
    }

    public Boolean getAutoHeadFeatureEnabled() {
        return autoHeadFeatureEnabled;
    }

    public void setAutoHeadFeatureEnabled(Boolean autoHeadFeatureEnabled) {
        this.autoHeadFeatureEnabled = autoHeadFeatureEnabled;
    }

    public Boolean getCompressionFeatureEnabled() {
        return compressionFeatureEnabled;
    }

    public void setCompressionFeatureEnabled(Boolean compressionFeatureEnabled) {
        this.compressionFeatureEnabled = compressionFeatureEnabled;
    }

    public Boolean getConditionalHeadersFeatureEnabled() {
        return conditionalHeadersFeatureEnabled;
    }

    public void setConditionalHeadersFeatureEnabled(Boolean conditionalHeadersFeatureEnabled) {
        this.conditionalHeadersFeatureEnabled = conditionalHeadersFeatureEnabled;
    }

    public Boolean getCorsFeatureEnabled() {
        return corsFeatureEnabled;
    }

    public void setCorsFeatureEnabled(Boolean corsFeatureEnabled) {
        this.corsFeatureEnabled = corsFeatureEnabled;
    }

    public String getHelp() {
        return "Generates a kotlin server.";
    }

    public Boolean getHstsFeatureEnabled() {
        return hstsFeatureEnabled;
    }

    public void setHstsFeatureEnabled(Boolean hstsFeatureEnabled) {
        this.hstsFeatureEnabled = hstsFeatureEnabled;
    }

    public String getName() {
        return "kotlin-server";
    }

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            this.setLibrary((String) additionalProperties.get(CodegenConstants.LIBRARY));
        }

        if (additionalProperties.containsKey(Constants.AUTOMATIC_HEAD_REQUESTS)) {
            setAutoHeadFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.AUTOMATIC_HEAD_REQUESTS));
        } else {
            additionalProperties.put(Constants.AUTOMATIC_HEAD_REQUESTS, getAutoHeadFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.CONDITIONAL_HEADERS)) {
            setConditionalHeadersFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.CONDITIONAL_HEADERS));
        } else {
            additionalProperties.put(Constants.CONDITIONAL_HEADERS, getConditionalHeadersFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.HSTS)) {
            setHstsFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.HSTS));
        } else {
            additionalProperties.put(Constants.HSTS, getHstsFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.CORS)) {
            setCorsFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.CORS));
        } else {
            additionalProperties.put(Constants.CORS, getCorsFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.COMPRESSION)) {
            setCompressionFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.COMPRESSION));
        } else {
            additionalProperties.put(Constants.COMPRESSION, getCompressionFeatureEnabled());
        }

        Boolean generateApis = additionalProperties.containsKey(CodegenConstants.GENERATE_APIS) && (Boolean)additionalProperties.get(CodegenConstants.GENERATE_APIS);
        String packageFolder = (sourceFolder + File.separator + packageName).replace(".", File.separator);
        String resourcesFolder = "src/main/resources"; // not sure this can be user configurable.

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));

        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("gradle.properties", "", "gradle.properties"));

        supportingFiles.add(new SupportingFile("AppMain.kt.mustache", packageFolder, "AppMain.kt"));
        supportingFiles.add(new SupportingFile("Configuration.kt.mustache", packageFolder, "Configuration.kt"));

        if (generateApis) {
            supportingFiles.add(new SupportingFile("Paths.kt.mustache", packageFolder, "Paths.kt"));
        }

        supportingFiles.add(new SupportingFile("application.conf.mustache", resourcesFolder, "application.conf"));
        supportingFiles.add(new SupportingFile("logback.xml", resourcesFolder, "logback.xml"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", File.separator);

        supportingFiles.add(new SupportingFile("ApiKeyAuth.kt.mustache", infrastructureFolder, "ApiKeyAuth.kt"));

        addMustacheLambdas(additionalProperties);
    }

    private void addMustacheLambdas(Map<String, Object> objs) {

        Map<String, Mustache.Lambda> lambdas = new ImmutableMap.Builder<String, Mustache.Lambda>()
                .put("lowercase", new LowercaseLambda().generator(this))
                .put("uppercase", new UppercaseLambda())
                .put("titlecase", new TitlecaseLambda())
                .put("camelcase", new CamelCaseLambda().generator(this))
                .put("indented", new IndentedLambda())
                .put("indented_8", new IndentedLambda(8, " "))
                .put("indented_12", new IndentedLambda(12, " "))
                .put("indented_16", new IndentedLambda(16, " "))
                .build();

        if (objs.containsKey("lambda")) {
            LOGGER.warn("An property named 'lambda' already exists. Mustache lambdas renamed from 'lambda' to '_lambda'. " +
                    "You'll likely need to use a custom template, " +
                    "see https://github.com/swagger-api/swagger-codegen#modifying-the-client-library-format. ");
            objs.put("_lambda", lambdas);
        } else {
            objs.put("lambda", lambdas);
        }
    }

    public static class Constants {
        public final static String KTOR = "ktor";
        public final static String AUTOMATIC_HEAD_REQUESTS = "featureAutoHead";
        public final static String AUTOMATIC_HEAD_REQUESTS_DESC = "Automatically provide responses to HEAD requests for existing routes that have the GET verb defined.";
        public final static String CONDITIONAL_HEADERS = "featureConditionalHeaders";
        public final static String CONDITIONAL_HEADERS_DESC = "Avoid sending content if client already has same content, by checking ETag or LastModified properties.";
        public final static String HSTS = "featureHSTS";
        public final static String HSTS_DESC = "Avoid sending content if client already has same content, by checking ETag or LastModified properties.";
        public final static String CORS = "featureCORS";
        public final static String CORS_DESC = "Ktor by default provides an interceptor for implementing proper support for Cross-Origin Resource Sharing (CORS). See enable-cors.org.";
        public final static String COMPRESSION = "featureCompression";
        public final static String COMPRESSION_DESC = "Adds ability to compress outgoing content using gzip, deflate or custom encoder and thus reduce size of the response.";
    }
}
