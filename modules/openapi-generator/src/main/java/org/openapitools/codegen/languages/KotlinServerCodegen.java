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

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;

public class KotlinServerCodegen extends AbstractKotlinCodegen {
    public static final String DEFAULT_LIBRARY = Constants.KTOR;
    private final Logger LOGGER = LoggerFactory.getLogger(KotlinServerCodegen.class);
    private Boolean autoHeadFeatureEnabled = true;
    private Boolean conditionalHeadersFeatureEnabled = false;
    private Boolean hstsFeatureEnabled = true;
    private Boolean corsFeatureEnabled = false;
    private Boolean compressionFeatureEnabled = true;
    private Boolean locationsFeatureEnabled = true;
    private Boolean metricsFeatureEnabled = true;

    // This is here to potentially warn the user when an option is not supported by the target framework.
    private Map<String, List<String>> optionsSupportedPerFramework = new ImmutableMap.Builder<String, List<String>>()
            .put(Constants.KTOR, Arrays.asList(
                    Constants.AUTOMATIC_HEAD_REQUESTS,
                    Constants.CONDITIONAL_HEADERS,
                    Constants.HSTS,
                    Constants.CORS,
                    Constants.COMPRESSION,
                    Constants.LOCATIONS,
                    Constants.METRICS
            ))
            .build();

    /**
     * Constructs an instance of `KotlinServerCodegen`.
     */
    public KotlinServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        artifactId = "kotlin-server";
        packageName = "org.openapitools.server";

        typeMapping.put("array", "kotlin.collections.List");

        // cliOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.artifactId);
        updateOption(CodegenConstants.PACKAGE_NAME, this.packageName);

        outputFolder = "generated-code" + File.separator + "kotlin-server";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        embeddedTemplateDir = templateDir = "kotlin-server";
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        supportedLibraries.put(Constants.KTOR, "ktor framework");

        // TODO: Configurable server engine. Defaults to netty in build.gradle.
        CliOption library = new CliOption(CodegenConstants.LIBRARY, CodegenConstants.LIBRARY_DESC);
        library.setDefault(DEFAULT_LIBRARY);
        library.setEnum(supportedLibraries);

        cliOptions.add(library);

        addSwitch(Constants.AUTOMATIC_HEAD_REQUESTS, Constants.AUTOMATIC_HEAD_REQUESTS_DESC, getAutoHeadFeatureEnabled());
        addSwitch(Constants.CONDITIONAL_HEADERS, Constants.CONDITIONAL_HEADERS_DESC, getConditionalHeadersFeatureEnabled());
        addSwitch(Constants.HSTS, Constants.HSTS_DESC, getHstsFeatureEnabled());
        addSwitch(Constants.CORS, Constants.CORS_DESC, getCorsFeatureEnabled());
        addSwitch(Constants.COMPRESSION, Constants.COMPRESSION_DESC, getCompressionFeatureEnabled());
        addSwitch(Constants.LOCATIONS, Constants.LOCATIONS_DESC, getLocationsFeatureEnabled());
        addSwitch(Constants.METRICS, Constants.METRICS_DESC, getMetricsFeatureEnabled());
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
        return "Generates a Kotlin server.";
    }

    public Boolean getHstsFeatureEnabled() {
        return hstsFeatureEnabled;
    }

    public void setHstsFeatureEnabled(Boolean hstsFeatureEnabled) {
        this.hstsFeatureEnabled = hstsFeatureEnabled;
    }

    public Boolean getLocationsFeatureEnabled() {
        return locationsFeatureEnabled;
    }

    public void setLocationsFeatureEnabled(Boolean locationsFeatureEnabled) {
        this.locationsFeatureEnabled = locationsFeatureEnabled;
    }

    public Boolean getMetricsFeatureEnabled() {
        return metricsFeatureEnabled;
    }

    public void setMetricsFeatureEnabled(Boolean metricsEnabled) {
        this.metricsFeatureEnabled = metricsEnabled;
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

        // set default library to "ktor"
        if (StringUtils.isEmpty(library)) {
            this.setLibrary(DEFAULT_LIBRARY);
            additionalProperties.put(CodegenConstants.LIBRARY, DEFAULT_LIBRARY);
            LOGGER.info("`library` option is empty. Default to {}", DEFAULT_LIBRARY);
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

        if (additionalProperties.containsKey(Constants.LOCATIONS)) {
            setLocationsFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.LOCATIONS));
        } else {
            additionalProperties.put(Constants.LOCATIONS, getLocationsFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.METRICS)) {
            setMetricsFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.METRICS));
        } else {
            additionalProperties.put(Constants.METRICS, getMetricsFeatureEnabled());
        }

        boolean generateApis = additionalProperties.containsKey(CodegenConstants.GENERATE_APIS) && (Boolean) additionalProperties.get(CodegenConstants.GENERATE_APIS);
        String packageFolder = (sourceFolder + File.separator + packageName).replace(".", File.separator);
        String resourcesFolder = "src/main/resources"; // not sure this can be user configurable.

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));

        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("gradle.properties", "", "gradle.properties"));

        supportingFiles.add(new SupportingFile("AppMain.kt.mustache", packageFolder, "AppMain.kt"));
        supportingFiles.add(new SupportingFile("Configuration.kt.mustache", packageFolder, "Configuration.kt"));

        if (generateApis && locationsFeatureEnabled) {
            supportingFiles.add(new SupportingFile("Paths.kt.mustache", packageFolder, "Paths.kt"));
        }

        supportingFiles.add(new SupportingFile("application.conf.mustache", resourcesFolder, "application.conf"));
        supportingFiles.add(new SupportingFile("logback.xml", resourcesFolder, "logback.xml"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", File.separator);

        supportingFiles.add(new SupportingFile("ApiKeyAuth.kt.mustache", infrastructureFolder, "ApiKeyAuth.kt"));
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
        public final static String LOCATIONS = "featureLocations";
        public final static String LOCATIONS_DESC = "Generates routes in a typed way, for both: constructing URLs and reading the parameters.";
        public final static String METRICS = "featureMetrics";
        public final static String METRICS_DESC = "Enables metrics feature.";
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# This generator's contributed by Jim Schubert (https://github.com/jimschubert)#");
        System.out.println("# Please support his work directly via https://patreon.com/jimschubert \uD83D\uDE4F     #");
        System.out.println("################################################################################");
    }
}
