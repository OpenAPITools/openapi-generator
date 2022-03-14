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

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.GzipFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.templating.mustache.CaseFormatLambda;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.google.common.base.CaseFormat.LOWER_CAMEL;
import static com.google.common.base.CaseFormat.UPPER_UNDERSCORE;
import static java.util.Collections.sort;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class JavaClientCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures, GzipFeatures {

    static final String MEDIA_TYPE = "mediaType";

    private final Logger LOGGER = LoggerFactory.getLogger(JavaClientCodegen.class);

    public static final String USE_RX_JAVA2 = "useRxJava2";
    public static final String USE_RX_JAVA3 = "useRxJava3";
    public static final String DO_NOT_USE_RX = "doNotUseRx";
    public static final String USE_PLAY_WS = "usePlayWS";
    public static final String ASYNC_NATIVE = "asyncNative";
    public static final String CONFIG_KEY = "configKey";
    public static final String PARCELABLE_MODEL = "parcelableModel";
    public static final String USE_RUNTIME_EXCEPTION = "useRuntimeException";
    public static final String USE_REFLECTION_EQUALS_HASHCODE = "useReflectionEqualsHashCode";
    public static final String CASE_INSENSITIVE_RESPONSE_HEADERS = "caseInsensitiveResponseHeaders";
    public static final String MICROPROFILE_FRAMEWORK = "microprofileFramework";
    public static final String USE_ABSTRACTION_FOR_FILES = "useAbstractionForFiles";
    public static final String DYNAMIC_OPERATIONS = "dynamicOperations";
    public static final String SUPPORT_STREAMING = "supportStreaming";
    public static final String GRADLE_PROPERTIES = "gradleProperties";
    public static final String ERROR_OBJECT_TYPE = "errorObjectType";
    public static final String ERROR_OBJECT_SUBTYPE = "errorObjectSubtype";

    public static final String MICROPROFILE_DEFAULT = "default";
    public static final String MICROPROFILE_KUMULUZEE = "kumuluzee";

    public static final String FEIGN = "feign";
    public static final String GOOGLE_API_CLIENT = "google-api-client";
    public static final String JERSEY1 = "jersey1";
    public static final String JERSEY2 = "jersey2";
    public static final String NATIVE = "native";
    public static final String OKHTTP_GSON = "okhttp-gson";
    public static final String RESTEASY = "resteasy";
    public static final String RESTTEMPLATE = "resttemplate";
    public static final String WEBCLIENT = "webclient";
    public static final String REST_ASSURED = "rest-assured";
    public static final String RETROFIT_2 = "retrofit2";
    public static final String VERTX = "vertx";
    public static final String MICROPROFILE = "microprofile";
    public static final String APACHE = "apache-httpclient";

    public static final String SERIALIZATION_LIBRARY_GSON = "gson";
    public static final String SERIALIZATION_LIBRARY_JACKSON = "jackson";
    public static final String SERIALIZATION_LIBRARY_JSONB = "jsonb";

    protected String gradleWrapperPackage = "gradle.wrapper";
    protected boolean useRxJava = false;
    protected boolean useRxJava2 = false;
    protected boolean useRxJava3 = false;
    // backwards compatibility for openapi configs that specify neither rx1 nor rx2
    // (mustache does not allow for boolean operators so we need this extra field)
    protected boolean doNotUseRx = true;
    protected boolean usePlayWS = false;
    protected String microprofileFramework = MICROPROFILE_DEFAULT;
    protected String configKey = null;

    protected boolean asyncNative = false;
    protected boolean parcelableModel = false;
    protected boolean useBeanValidation = false;
    protected boolean performBeanValidation = false;
    protected boolean useGzipFeature = false;
    protected boolean useRuntimeException = false;
    protected boolean useReflectionEqualsHashCode = false;
    protected boolean caseInsensitiveResponseHeaders = false;
    protected boolean useAbstractionForFiles = false;
    protected boolean dynamicOperations = false;
    protected boolean supportStreaming = false;
    protected String gradleProperties;
    protected String errorObjectType;
    protected List<String> errorObjectSubtype;
    protected String authFolder;
    protected String serializationLibrary = null;
    protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup

    public JavaClientCodegen() {
        super();

        // TODO: Move GlobalFeature.ParameterizedServer to library: jersey after moving featureSet to generatorMetadata
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .includeGlobalFeatures(GlobalFeature.ParameterizedServer)
        );

        outputFolder = "generated-code" + File.separator + "java";
        embeddedTemplateDir = templateDir = "Java";
        invokerPackage = "org.openapitools.client";
        artifactId = "openapi-java-client";
        apiPackage = "org.openapitools.client.api";
        modelPackage = "org.openapitools.client.model";

        // cliOptions default redefinition need to be updated
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        modelTestTemplateFiles.put("model_test.mustache", ".java");

        cliOptions.add(CliOption.newBoolean(USE_RX_JAVA2, "Whether to use the RxJava2 adapter with the retrofit2 library. IMPORTANT: This option has been deprecated."));
        cliOptions.add(CliOption.newBoolean(USE_RX_JAVA3, "Whether to use the RxJava3 adapter with the retrofit2 library. IMPORTANT: This option has been deprecated."));
        cliOptions.add(CliOption.newBoolean(PARCELABLE_MODEL, "Whether to generate models for Android that implement Parcelable with the okhttp-gson library."));
        cliOptions.add(CliOption.newBoolean(USE_PLAY_WS, "Use Play! Async HTTP client (Play WS API)"));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));
        cliOptions.add(CliOption.newBoolean(PERFORM_BEANVALIDATION, "Perform BeanValidation"));
        cliOptions.add(CliOption.newBoolean(USE_GZIP_FEATURE, "Send gzip-encoded requests"));
        cliOptions.add(CliOption.newBoolean(USE_RUNTIME_EXCEPTION, "Use RuntimeException instead of Exception"));
        cliOptions.add(CliOption.newBoolean(ASYNC_NATIVE, "If true, async handlers will be used, instead of the sync version"));
        cliOptions.add(CliOption.newBoolean(USE_REFLECTION_EQUALS_HASHCODE, "Use org.apache.commons.lang3.builder for equals and hashCode in the models. WARNING: This will fail under a security manager, unless the appropriate permissions are set up correctly and also there's potential performance impact."));
        cliOptions.add(CliOption.newBoolean(CASE_INSENSITIVE_RESPONSE_HEADERS, "Make API response's headers case-insensitive. Available on " + OKHTTP_GSON + ", " + JERSEY2 + " libraries"));
        cliOptions.add(CliOption.newString(MICROPROFILE_FRAMEWORK, "Framework for microprofile. Possible values \"kumuluzee\""));
        cliOptions.add(CliOption.newBoolean(USE_ABSTRACTION_FOR_FILES, "Use alternative types instead of java.io.File to allow passing bytes without a file on disk. Available on resttemplate, webclient, libraries"));
        cliOptions.add(CliOption.newBoolean(DYNAMIC_OPERATIONS, "Generate operations dynamically at runtime from an OAS", this.dynamicOperations));
        cliOptions.add(CliOption.newBoolean(SUPPORT_STREAMING, "Support streaming endpoint (beta)", this.supportStreaming));
        cliOptions.add(CliOption.newString(GRADLE_PROPERTIES, "Append additional Gradle properties to the gradle.properties file"));
        cliOptions.add(CliOption.newString(ERROR_OBJECT_TYPE, "Error Object type. (This option is for okhttp-gson-next-gen only)"));
        cliOptions.add(CliOption.newString(CONFIG_KEY, "Config key in @RegisterRestClient. Default to none. Only `microprofile` supports this option."));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP_DESC + " Only jersey2, native, okhttp-gson support this option."));

        supportedLibraries.put(JERSEY1, "HTTP client: Jersey client 1.19.x. JSON processing: Jackson 2.9.x. Enable gzip request encoding using '-DuseGzipFeature=true'. IMPORTANT NOTE: jersey 1.x is no longer actively maintained so please upgrade to 'jersey2' or other HTTP libraries instead.");
        supportedLibraries.put(JERSEY2, "HTTP client: Jersey client 2.25.1. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(FEIGN, "HTTP client: OpenFeign 10.x. JSON processing: Jackson 2.9.x.");
        supportedLibraries.put(OKHTTP_GSON, "[DEFAULT] HTTP client: OkHttp 3.x. JSON processing: Gson 2.8.x. Enable Parcelable models on Android using '-DparcelableModel=true'. Enable gzip request encoding using '-DuseGzipFeature=true'.");
        supportedLibraries.put(RETROFIT_2, "HTTP client: OkHttp 3.x. JSON processing: Gson 2.x (Retrofit 2.3.0). Enable the RxJava adapter using '-DuseRxJava[2/3]=true'. (RxJava 1.x or 2.x or 3.x)");
        supportedLibraries.put(RESTTEMPLATE, "HTTP client: Spring RestTemplate 4.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(WEBCLIENT, "HTTP client: Spring WebClient 5.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(RESTEASY, "HTTP client: Resteasy client 3.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(VERTX, "HTTP client: VertX client 3.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(GOOGLE_API_CLIENT, "HTTP client: Google API client 1.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(REST_ASSURED, "HTTP client: rest-assured : 4.x. JSON processing: Gson 2.x or Jackson 2.10.x. Only for Java 8");
        supportedLibraries.put(NATIVE, "HTTP client: Java native HttpClient. JSON processing: Jackson 2.9.x. Only for Java11+");
        supportedLibraries.put(MICROPROFILE, "HTTP client: Microprofile client 1.x. JSON processing: JSON-B");
        supportedLibraries.put(APACHE, "HTTP client: Apache httpclient 4.x");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        // set okhttp-gson as the default
        libraryOption.setDefault(OKHTTP_GSON);
        cliOptions.add(libraryOption);
        setLibrary(OKHTTP_GSON);

        CliOption serializationLibrary = new CliOption(CodegenConstants.SERIALIZATION_LIBRARY, "Serialization library, default depends on value of the option library");
        Map<String, String> serializationOptions = new HashMap<>();
        serializationOptions.put(SERIALIZATION_LIBRARY_GSON, "Use Gson as serialization library");
        serializationOptions.put(SERIALIZATION_LIBRARY_JACKSON, "Use Jackson as serialization library");
        serializationOptions.put(SERIALIZATION_LIBRARY_JSONB, "Use JSON-B as serialization library");
        serializationLibrary.setEnum(serializationOptions);
        cliOptions.add(serializationLibrary);

        // Ensure the OAS 3.x discriminator mappings include any descendent schemas that allOf
        // inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values,
        // and the discriminator mapping schemas in the OAS document.
        this.setLegacyDiscriminatorBehavior(false);

    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "java";
    }

    @Override
    public String getHelp() {
        return "Generates a Java client library (HTTP lib: Jersey (1.x, 2.x), Retrofit (2.x), OpenFeign (10.x) and more.";
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);
        if (MICROPROFILE.equals(getLibrary())) {
            co.subresourceOperation = !co.path.isEmpty();
        }
    }

    @Override
    public void processOpts() {
        if (WEBCLIENT.equals(getLibrary()) || NATIVE.equals(getLibrary())) {
            dateLibrary = "java8";
        } else if (MICROPROFILE.equals(getLibrary())) {
            dateLibrary = "legacy";
        }
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP)) {
            setUseOneOfDiscriminatorLookup(convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP));
        } else {
            additionalProperties.put(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, useOneOfDiscriminatorLookup);
        }

        // RxJava
        if (additionalProperties.containsKey(USE_RX_JAVA2) && additionalProperties.containsKey(USE_RX_JAVA3)) {
            LOGGER.warn("You specified all RxJava versions 2 and 3 but they are mutually exclusive. Defaulting to v3.");
            this.setUseRxJava3(Boolean.parseBoolean(additionalProperties.get(USE_RX_JAVA3).toString()));
        } else {
            if (additionalProperties.containsKey(USE_RX_JAVA2) && additionalProperties.containsKey(USE_RX_JAVA3)) {
                LOGGER.warn("You specified both RxJava versions 2 and 3 but they are mutually exclusive. Defaulting to v3.");
                this.setUseRxJava3(Boolean.parseBoolean(additionalProperties.get(USE_RX_JAVA3).toString()));
            } else {
                if (additionalProperties.containsKey(USE_RX_JAVA2)) {
                    this.setUseRxJava2(Boolean.parseBoolean(additionalProperties.get(USE_RX_JAVA2).toString()));
                }
                if (additionalProperties.containsKey(USE_RX_JAVA3)) {
                    this.setUseRxJava3(Boolean.parseBoolean(additionalProperties.get(USE_RX_JAVA3).toString()));
                }
            }
        }

        if (!useRxJava && !useRxJava2 && !useRxJava3) {
            additionalProperties.put(DO_NOT_USE_RX, true);
        }

        // Java Play
        if (additionalProperties.containsKey(USE_PLAY_WS)) {
            this.setUsePlayWS(Boolean.parseBoolean(additionalProperties.get(USE_PLAY_WS).toString()));
        }
        additionalProperties.put(USE_PLAY_WS, usePlayWS);

        // Microprofile framework
        if (additionalProperties.containsKey(MICROPROFILE_FRAMEWORK)) {
            if (!MICROPROFILE_KUMULUZEE.equals(microprofileFramework)) {
                throw new RuntimeException("Invalid microprofileFramework '" + microprofileFramework + "'. Must be 'kumuluzee' or none.");
            }
            this.setMicroprofileFramework(additionalProperties.get(MICROPROFILE_FRAMEWORK).toString());
        }
        additionalProperties.put(MICROPROFILE_FRAMEWORK, microprofileFramework);

        if (additionalProperties.containsKey(CONFIG_KEY)) {
            this.setConfigKey(additionalProperties.get(CONFIG_KEY).toString());
        }

        if (additionalProperties.containsKey(ASYNC_NATIVE)) {
            this.setAsyncNative(convertPropertyToBooleanAndWriteBack(ASYNC_NATIVE));
        }

        if (additionalProperties.containsKey(PARCELABLE_MODEL)) {
            this.setParcelableModel(Boolean.parseBoolean(additionalProperties.get(PARCELABLE_MODEL).toString()));
        }
        // put the boolean value back to PARCELABLE_MODEL in additionalProperties
        additionalProperties.put(PARCELABLE_MODEL, parcelableModel);

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            this.setUseBeanValidation(convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION));
        }

        if (additionalProperties.containsKey(PERFORM_BEANVALIDATION)) {
            this.setPerformBeanValidation(convertPropertyToBooleanAndWriteBack(PERFORM_BEANVALIDATION));
        }

        if (additionalProperties.containsKey(USE_GZIP_FEATURE)) {
            this.setUseGzipFeature(convertPropertyToBooleanAndWriteBack(USE_GZIP_FEATURE));
        }

        if (additionalProperties.containsKey(USE_RUNTIME_EXCEPTION)) {
            this.setUseRuntimeException(convertPropertyToBooleanAndWriteBack(USE_RUNTIME_EXCEPTION));
        }

        if (additionalProperties.containsKey(USE_REFLECTION_EQUALS_HASHCODE)) {
            this.setUseReflectionEqualsHashCode(convertPropertyToBooleanAndWriteBack(USE_REFLECTION_EQUALS_HASHCODE));
        }

        if (additionalProperties.containsKey(CASE_INSENSITIVE_RESPONSE_HEADERS)) {
            this.setUseReflectionEqualsHashCode(convertPropertyToBooleanAndWriteBack(CASE_INSENSITIVE_RESPONSE_HEADERS));
        }

        if (additionalProperties.containsKey(USE_ABSTRACTION_FOR_FILES)) {
            this.setUseAbstractionForFiles(convertPropertyToBooleanAndWriteBack(USE_ABSTRACTION_FOR_FILES));
        }

        if (additionalProperties.containsKey(DYNAMIC_OPERATIONS)) {
            this.setDynamicOperations(Boolean.parseBoolean(additionalProperties.get(DYNAMIC_OPERATIONS).toString()));
        }
        additionalProperties.put(DYNAMIC_OPERATIONS, dynamicOperations);

        if (additionalProperties.containsKey(SUPPORT_STREAMING)) {
            this.setSupportStreaming(Boolean.parseBoolean(additionalProperties.get(SUPPORT_STREAMING).toString()));
        }
        additionalProperties.put(SUPPORT_STREAMING, supportStreaming);

        if (additionalProperties.containsKey(GRADLE_PROPERTIES)) {
            this.setGradleProperties(additionalProperties.get(GRADLE_PROPERTIES).toString());
        }
        additionalProperties.put(GRADLE_PROPERTIES, gradleProperties);

        if (additionalProperties.containsKey(ERROR_OBJECT_TYPE)) {
            this.setErrorObjectType(additionalProperties.get(ERROR_OBJECT_TYPE).toString());
        }
        additionalProperties.put(ERROR_OBJECT_TYPE, errorObjectType);

        if (additionalProperties.containsKey(ERROR_OBJECT_SUBTYPE)) {
            this.setErrorObjectSubtype((List<String>) additionalProperties.get(ERROR_OBJECT_SUBTYPE));
        }
        additionalProperties.put(ERROR_OBJECT_SUBTYPE, errorObjectSubtype);

        final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");
        final String apiFolder = (sourceFolder + '/' + apiPackage).replace(".", "/");
        final String modelsFolder = (sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
        authFolder = (sourceFolder + '/' + invokerPackage + ".auth").replace(".", "/");

        //Common files
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml").doNotOverwrite());
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md").doNotOverwrite());
        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle").doNotOverwrite());
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt").doNotOverwrite());
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle").doNotOverwrite());
        supportingFiles.add(new SupportingFile("gradle.properties.mustache", "", "gradle.properties").doNotOverwrite());
        supportingFiles.add(new SupportingFile("manifest.mustache", projectFolder, "AndroidManifest.xml").doNotOverwrite());
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache", invokerFolder, "ApiClient.java"));
        supportingFiles.add(new SupportingFile("ServerConfiguration.mustache", invokerFolder, "ServerConfiguration.java"));
        supportingFiles.add(new SupportingFile("ServerVariable.mustache", invokerFolder, "ServerVariable.java"));
        supportingFiles.add(new SupportingFile("maven.yml.mustache", ".github/workflows", "maven.yml"));
        if (dynamicOperations) {
            supportingFiles.add(new SupportingFile("openapi.mustache", projectFolder + "/resources/openapi", "openapi.yaml"));
            supportingFiles.add(new SupportingFile("apiOperation.mustache", invokerFolder, "ApiOperation.java"));
        } else {
            supportingFiles.add(new SupportingFile("openapi.mustache", "api", "openapi.yaml"));
        }

        // helper for client library that allow to parse/format java.time.OffsetDateTime or org.threeten.bp.OffsetDateTime
        if (additionalProperties.containsKey("jsr310") && (isLibrary(WEBCLIENT) || isLibrary(VERTX) || isLibrary(RESTTEMPLATE) || isLibrary(RESTEASY) || isLibrary(MICROPROFILE) || isLibrary(JERSEY1) || isLibrary(JERSEY2) || isLibrary(APACHE))) {
            supportingFiles.add(new SupportingFile("JavaTimeFormatter.mustache", invokerFolder, "JavaTimeFormatter.java"));
        }

        if (!(RESTTEMPLATE.equals(getLibrary()) || isLibrary(REST_ASSURED) || isLibrary(NATIVE) || isLibrary(MICROPROFILE))) {
            supportingFiles.add(new SupportingFile("StringUtil.mustache", invokerFolder, "StringUtil.java"));
        }

        // google-api-client doesn't use the OpenAPI auth, because it uses Google Credential directly (HttpRequestInitializer)
        if (!(isLibrary(GOOGLE_API_CLIENT) || isLibrary(REST_ASSURED) || isLibrary(NATIVE) || isLibrary(MICROPROFILE))) {
            supportingFiles.add(new SupportingFile("auth/HttpBasicAuth.mustache", authFolder, "HttpBasicAuth.java"));
            supportingFiles.add(new SupportingFile("auth/HttpBearerAuth.mustache", authFolder, "HttpBearerAuth.java"));
            supportingFiles.add(new SupportingFile("auth/ApiKeyAuth.mustache", authFolder, "ApiKeyAuth.java"));
        }

        supportingFiles.add(new SupportingFile("gradlew.mustache", "", "gradlew"));
        supportingFiles.add(new SupportingFile("gradlew.bat.mustache", "", "gradlew.bat"));
        supportingFiles.add(new SupportingFile("gradle-wrapper.properties.mustache",
                gradleWrapperPackage.replace(".", File.separator), "gradle-wrapper.properties"));
        supportingFiles.add(new SupportingFile("gradle-wrapper.jar",
                gradleWrapperPackage.replace(".", File.separator), "gradle-wrapper.jar"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        if (performBeanValidation) {
            supportingFiles.add(new SupportingFile("BeanValidationException.mustache", invokerFolder,
                    "BeanValidationException.java"));
        }

        if (additionalProperties.containsKey(CodegenConstants.SERIALIZATION_LIBRARY)) {
            setSerializationLibrary(additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY).toString());
        }

        //TODO: add auto-generated doc to feign
        if (FEIGN.equals(getLibrary())) {
            modelDocTemplateFiles.remove("model_doc.mustache");
            apiDocTemplateFiles.remove("api_doc.mustache");
            //Templates to decode response headers
            supportingFiles.add(new SupportingFile("model/ApiResponse.mustache", modelsFolder, "ApiResponse.java"));
            supportingFiles.add(new SupportingFile("ApiResponseDecoder.mustache", invokerFolder, "ApiResponseDecoder.java"));

            // TODO remove "file" from reserved word list as feign client doesn't support using `baseName`
            // as the parameter name yet
            reservedWords.remove("file");
        }

        if (!(FEIGN.equals(getLibrary()) || RESTTEMPLATE.equals(getLibrary()) || RETROFIT_2.equals(getLibrary()) || GOOGLE_API_CLIENT.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || WEBCLIENT.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) {
            supportingFiles.add(new SupportingFile("apiException.mustache", invokerFolder, "ApiException.java"));
            supportingFiles.add(new SupportingFile("Configuration.mustache", invokerFolder, "Configuration.java"));
            supportingFiles.add(new SupportingFile("Pair.mustache", invokerFolder, "Pair.java"));
        }

        if (!(FEIGN.equals(getLibrary()) || RESTTEMPLATE.equals(getLibrary()) || RETROFIT_2.equals(getLibrary()) || GOOGLE_API_CLIENT.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || NATIVE.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) {
            supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));
        }

        if (FEIGN.equals(getLibrary())) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
            supportingFiles.add(new SupportingFile("ParamExpander.mustache", invokerFolder, "ParamExpander.java"));
            supportingFiles.add(new SupportingFile("EncodingUtils.mustache", invokerFolder, "EncodingUtils.java"));
            supportingFiles.add(new SupportingFile("auth/DefaultApi20Impl.mustache", authFolder, "DefaultApi20Impl.java"));
        } else if (OKHTTP_GSON.equals(getLibrary()) || StringUtils.isEmpty(getLibrary())) {
            // the "okhttp-gson" library template requires "ApiCallback.mustache" for async call
            supportingFiles.add(new SupportingFile("ApiCallback.mustache", invokerFolder, "ApiCallback.java"));
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("ProgressRequestBody.mustache", invokerFolder, "ProgressRequestBody.java"));
            supportingFiles.add(new SupportingFile("ProgressResponseBody.mustache", invokerFolder, "ProgressResponseBody.java"));
            supportingFiles.add(new SupportingFile("GzipRequestInterceptor.mustache", invokerFolder, "GzipRequestInterceptor.java"));
            if (OKHTTP_GSON.equals(getLibrary())) {
                supportingFiles.add(new SupportingFile("AbstractOpenApiSchema.mustache", modelsFolder, "AbstractOpenApiSchema.java"));
            }

            // NOTE: below moved to postProcessOperationsWithModels
            //supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
            //supportingFiles.add(new SupportingFile("auth/RetryingOAuth.mustache", authFolder, "RetryingOAuth.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_GSON);
        } else if (RETROFIT_2.equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
            supportingFiles.add(new SupportingFile("CollectionFormats.mustache", invokerFolder, "CollectionFormats.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_GSON);
            if (RETROFIT_2.equals(getLibrary()) && !usePlayWS) {
                supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            }
        } else if (JERSEY2.equals(getLibrary())) {
            additionalProperties.put("jersey2", true);
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            if (ProcessUtils.hasHttpSignatureMethods(openAPI)) {
                supportingFiles.add(new SupportingFile("auth/HttpSignatureAuth.mustache", authFolder, "HttpSignatureAuth.java"));
            }
            supportingFiles.add(new SupportingFile("AbstractOpenApiSchema.mustache", modelsFolder, "AbstractOpenApiSchema.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);

            // Composed schemas can have the 'additionalProperties' keyword, as specified in JSON schema.
            // In principle, this should be enabled by default for all code generators. However due to limitations
            // in other code generators, support needs to be enabled on a case-by-case basis.
            // The flag below should be set for all Java libraries, but the templates need to be ported
            // one by one for each library.
            supportsAdditionalPropertiesWithComposedSchema = true;

        } else if (NATIVE.equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("AbstractOpenApiSchema.mustache", modelsFolder, "AbstractOpenApiSchema.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        } else if (RESTEASY.equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        } else if (JERSEY1.equals(getLibrary())) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        } else if (RESTTEMPLATE.equals(getLibrary())) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
            supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));
        } else if (WEBCLIENT.equals(getLibrary())) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        } else if (VERTX.equals(getLibrary())) {
            typeMapping.put("file", "AsyncFile");
            importMapping.put("AsyncFile", "io.vertx.core.file.AsyncFile");
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
            apiTemplateFiles.put("apiImpl.mustache", "Impl.java");
            apiTemplateFiles.put("rxApiImpl.mustache", ".java");
            supportingFiles.remove(new SupportingFile("manifest.mustache", projectFolder, "AndroidManifest.xml"));
        } else if (GOOGLE_API_CLIENT.equals(getLibrary())) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);

        } else if (REST_ASSURED.equals(getLibrary())) {
            if (getSerializationLibrary() == null) {
                LOGGER.info("No serializationLibrary configured, using '{}' as fallback", SERIALIZATION_LIBRARY_GSON);
                setSerializationLibrary(SERIALIZATION_LIBRARY_GSON);
            }
            if (SERIALIZATION_LIBRARY_JACKSON.equals(getSerializationLibrary())) {
                supportingFiles.add(new SupportingFile("JacksonObjectMapper.mustache", invokerFolder, "JacksonObjectMapper.java"));
            } else if (SERIALIZATION_LIBRARY_GSON.equals(getSerializationLibrary())) {
                supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
                supportingFiles.add(new SupportingFile("GsonObjectMapper.mustache", invokerFolder, "GsonObjectMapper.java"));
            }
            supportingFiles.add(new SupportingFile("Oper.mustache", apiFolder, "Oper.java"));
            additionalProperties.put("convert", new CaseFormatLambda(LOWER_CAMEL, UPPER_UNDERSCORE));
            apiTemplateFiles.put("api.mustache", ".java");
            supportingFiles.add(new SupportingFile("ResponseSpecBuilders.mustache", invokerFolder, "ResponseSpecBuilders.java"));
        } else if (MICROPROFILE.equals(getLibrary())) {
            supportingFiles.clear(); // Don't need extra files provided by Java Codegen
            String apiExceptionFolder = (sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
            supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
            supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
            supportingFiles.add(new SupportingFile("api_exception.mustache", apiExceptionFolder, "ApiException.java"));
            supportingFiles.add(new SupportingFile("api_exception_mapper.mustache", apiExceptionFolder, "ApiExceptionMapper.java"));
            serializationLibrary = "none";

            if (microprofileFramework.equals(MICROPROFILE_KUMULUZEE)) {
                supportingFiles.add(new SupportingFile("kumuluzee.pom.mustache", "", "pom.xml"));
                supportingFiles.add(new SupportingFile("kumuluzee.config.yaml.mustache", "src/main/resources", "config.yaml"));
                supportingFiles.add(new SupportingFile("kumuluzee.beans.xml.mustache", "src/main/resources/META-INF", "beans.xml"));
            }
        } else if (APACHE.equals(getLibrary())) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        } else {
            LOGGER.error("Unknown library option (-l/--library): {}", getLibrary());
        }

        if (usePlayWS) {
            // remove unsupported auth
            Iterator<SupportingFile> iter = supportingFiles.iterator();
            while (iter.hasNext()) {
                SupportingFile sf = iter.next();
                if (sf.getTemplateFile().startsWith("auth/")) {
                    iter.remove();
                }
            }

            apiTemplateFiles.remove("api.mustache");
            apiTemplateFiles.put("play26/api.mustache", ".java");

            supportingFiles.add(new SupportingFile("play26/ApiClient.mustache", invokerFolder, "ApiClient.java"));
            supportingFiles.add(new SupportingFile("play26/Play26CallFactory.mustache", invokerFolder, "Play26CallFactory.java"));
            supportingFiles.add(new SupportingFile("play26/Play26CallAdapterFactory.mustache", invokerFolder,
                    "Play26CallAdapterFactory.java"));

            supportingFiles.add(new SupportingFile("play-common/auth/ApiKeyAuth.mustache", authFolder, "ApiKeyAuth.java"));
            supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));
            supportingFiles.add(new SupportingFile("Pair.mustache", invokerFolder, "Pair.java"));

            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        }

        if (getSerializationLibrary() == null) {
            LOGGER.info("No serializationLibrary configured, using '{}' as fallback", SERIALIZATION_LIBRARY_GSON);
            setSerializationLibrary(SERIALIZATION_LIBRARY_GSON);
        }
        switch (getSerializationLibrary()) {
            case SERIALIZATION_LIBRARY_JACKSON:
                additionalProperties.put(SERIALIZATION_LIBRARY_JACKSON, "true");
                additionalProperties.remove(SERIALIZATION_LIBRARY_GSON);
                additionalProperties.remove(SERIALIZATION_LIBRARY_JSONB);
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache", invokerFolder, "RFC3339DateFormat.java"));
                break;
            case SERIALIZATION_LIBRARY_GSON:
                additionalProperties.put(SERIALIZATION_LIBRARY_GSON, "true");
                additionalProperties.remove(SERIALIZATION_LIBRARY_JACKSON);
                additionalProperties.remove(SERIALIZATION_LIBRARY_JSONB);
                break;
            case SERIALIZATION_LIBRARY_JSONB:
                additionalProperties.put(SERIALIZATION_LIBRARY_JSONB, "true");
                additionalProperties.remove(SERIALIZATION_LIBRARY_JACKSON);
                additionalProperties.remove(SERIALIZATION_LIBRARY_GSON);
                break;
            default:
                additionalProperties.remove(SERIALIZATION_LIBRARY_JACKSON);
                additionalProperties.remove(SERIALIZATION_LIBRARY_GSON);
                additionalProperties.remove(SERIALIZATION_LIBRARY_JSONB);
                break;
        }

        // authentication related files
        // has OAuth defined
        if (ProcessUtils.hasOAuthMethods(openAPI)) {
            // for okhttp-gson (default), check to see if OAuth is defined and included OAuth-related files accordingly
            if ((OKHTTP_GSON.equals(getLibrary()) || StringUtils.isEmpty(getLibrary()))) {
                supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
                supportingFiles.add(new SupportingFile("auth/RetryingOAuth.mustache", authFolder, "RetryingOAuth.java"));
            }

            // google-api-client doesn't use the OpenAPI auth, because it uses Google Credential directly (HttpRequestInitializer)
            if (!(GOOGLE_API_CLIENT.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || usePlayWS
                    || NATIVE.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) {
                supportingFiles.add(new SupportingFile("auth/OAuth.mustache", authFolder, "OAuth.java"));
                supportingFiles.add(new SupportingFile("auth/OAuthFlow.mustache", authFolder, "OAuthFlow.java"));
            }

            // Add OauthPasswordGrant.java and OauthClientCredentialsGrant.java for feign library
            if (FEIGN.equals(getLibrary())) {
                supportingFiles.add(new SupportingFile("auth/OauthPasswordGrant.mustache", authFolder, "OauthPasswordGrant.java"));
                supportingFiles.add(new SupportingFile("auth/OauthClientCredentialsGrant.mustache", authFolder, "OauthClientCredentialsGrant.java"));
                supportingFiles.add(new SupportingFile("auth/ApiErrorDecoder.mustache", authFolder, "ApiErrorDecoder.java"));
            }
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        if (RETROFIT_2.equals(getLibrary())) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    if (operation.hasConsumes == Boolean.TRUE) {

                        if (isMultipartType(operation.consumes)) {
                            operation.isMultipart = Boolean.TRUE;
                        } else {
                            operation.prioritizedContentTypes = prioritizeContentTypes(operation.consumes);
                        }
                    }

                    if (StringUtils.isNotEmpty(operation.path) && operation.path.startsWith("/")) {
                        operation.path = operation.path.substring(1);
                    }

                    // sorting operation parameters to make sure path params are parsed before query params
                    if (operation.allParams != null) {
                        sort(operation.allParams, new Comparator<CodegenParameter>() {
                            @Override
                            public int compare(CodegenParameter one, CodegenParameter another) {
                                if (one.isPathParam && another.isQueryParam) {
                                    return -1;
                                }
                                if (one.isQueryParam && another.isPathParam) {
                                    return 1;
                                }

                                return 0;
                            }
                        });
                    }
                }
            }

        }

        // camelize path variables for Feign client
        if (FEIGN.equals(getLibrary())) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
            Pattern methodPattern = Pattern.compile("^(.*):([^:]*)$");
            for (CodegenOperation op : operationList) {
                String path = op.path;
                String method = "";

                // if a custom method is found at the end of the path, cut it off for later
                Matcher m = methodPattern.matcher(path);
                if (m.find()) {
                    path = m.group(1);
                    method = m.group(2);
                }

                String[] items = path.split("/", -1);

                for (int i = 0; i < items.length; ++i) {
                    if (items[i].matches("^\\{(.*)\\}$")) { // wrap in {}
                        // camelize path variable
                        items[i] = "{" + camelize(items[i].substring(1, items[i].length() - 1), true) + "}";
                    }
                }
                op.path = StringUtils.join(items, "/");
                // Replace the custom method on the path if one was found earlier
                if (!method.isEmpty()) {
                    op.path += ":" + method;
                }
            }
        }

        if (MICROPROFILE.equals(getLibrary())) {
            objs = AbstractJavaJAXRSServerCodegen.jaxrsPostProcessOperations(objs);
        }

        return objs;
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        if (VERTX.equals(getLibrary())) {
            String suffix = apiTemplateFiles().get(templateName);
            String subFolder = "";
            if (templateName.startsWith("rx")) {
                subFolder = "/rxjava";
            }
            return apiFileFolder() + subFolder + '/' + toApiFilename(tag) + suffix;
        } else {
            return super.apiFilename(templateName, tag);
        }
    }

    /**
     * Prioritizes consumes mime-type list by moving json-vendor and json mime-types up front, but
     * otherwise preserves original consumes definition order.
     * [application/vnd...+json,... application/json, ..as is..]
     *
     * @param consumes consumes mime-type list
     * @return
     */
    static List<Map<String, String>> prioritizeContentTypes(List<Map<String, String>> consumes) {
        if (consumes.size() <= 1)
            return consumes;

        List<Map<String, String>> prioritizedContentTypes = new ArrayList<>(consumes.size());

        List<Map<String, String>> jsonVendorMimeTypes = new ArrayList<>(consumes.size());
        List<Map<String, String>> jsonMimeTypes = new ArrayList<>(consumes.size());

        for (Map<String, String> consume : consumes) {
            if (isJsonVendorMimeType(consume.get(MEDIA_TYPE))) {
                jsonVendorMimeTypes.add(consume);
            } else if (isJsonMimeType(consume.get(MEDIA_TYPE))) {
                jsonMimeTypes.add(consume);
            } else
                prioritizedContentTypes.add(consume);
        }

        prioritizedContentTypes.addAll(0, jsonMimeTypes);
        prioritizedContentTypes.addAll(0, jsonVendorMimeTypes);
        return prioritizedContentTypes;
    }

    private static boolean isMultipartType(List<Map<String, String>> consumes) {
        Map<String, String> firstType = consumes.get(0);
        if (firstType != null) {
            if ("multipart/form-data".equals(firstType.get(MEDIA_TYPE))) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (!BooleanUtils.toBoolean(model.isEnum)) {
            //final String lib = getLibrary();
            //Needed imports for Jackson based libraries
            if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_JACKSON)) {
                model.imports.add("JsonProperty");
                model.imports.add("JsonValue");
                model.imports.add("JsonInclude");
                model.imports.add("JsonTypeName");
            }
            if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_GSON)) {
                model.imports.add("SerializedName");
                model.imports.add("TypeAdapter");
                model.imports.add("JsonAdapter");
                model.imports.add("JsonReader");
                model.imports.add("JsonWriter");
                model.imports.add("IOException");
            }
        } else { // enum class
            //Needed imports for Jackson's JsonCreator
            if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_JACKSON)) {
                model.imports.add("JsonValue");
                model.imports.add("JsonCreator");
            }
        }
        if (MICROPROFILE.equals(getLibrary())) {
            model.imports.remove("ApiModelProperty");
            model.imports.remove("ApiModel");
            model.imports.remove("JsonSerialize");
            model.imports.remove("ToStringSerializer");
        }

        if ("set".equals(property.containerType) && !JACKSON.equals(serializationLibrary)) {
            // clean-up
            model.imports.remove("JsonDeserialize");
            property.vendorExtensions.remove("x-setter-extra-annotation");
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        if (MICROPROFILE.equals(getLibrary())) {
            if (codegenModel.imports.contains("ApiModel")) {
                // Remove io.swagger.annotations.ApiModel import
                codegenModel.imports.remove("ApiModel");
            }
        }
        return codegenModel;
    }

    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);
        //Needed import for Gson based libraries
        if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_GSON)) {
            List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
            List<Object> models = (List<Object>) objs.get("models");
            for (Object _mo : models) {
                Map<String, Object> mo = (Map<String, Object>) _mo;
                CodegenModel cm = (CodegenModel) mo.get("model");
                // for enum model
                if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
                    cm.imports.add(importMapping.get("SerializedName"));
                    Map<String, String> item = new HashMap<String, String>();
                    item.put("import", importMapping.get("SerializedName"));
                    imports.add(item);
                }
            }
        }
        return objs;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = super.postProcessModels(objs);
        List<Object> models = (List<Object>) objs.get("models");

        if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_JACKSON) && !JERSEY1.equals(getLibrary())) {
            List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
            for (Object _mo : models) {
                Map<String, Object> mo = (Map<String, Object>) _mo;
                CodegenModel cm = (CodegenModel) mo.get("model");
                boolean addImports = false;

                for (CodegenProperty var : cm.vars) {
                    if (this.openApiNullable) {
                        boolean isOptionalNullable = Boolean.FALSE.equals(var.required) && Boolean.TRUE.equals(var.isNullable);
                        // only add JsonNullable and related imports to optional and nullable values
                        addImports |= isOptionalNullable;
                        var.getVendorExtensions().put("x-is-jackson-optional-nullable", isOptionalNullable);
                    }

                    if (Boolean.TRUE.equals(var.getVendorExtensions().get("x-enum-as-string"))) {
                        // treat enum string as just string
                        var.datatypeWithEnum = var.dataType;

                        if (StringUtils.isNotEmpty(var.defaultValue)) { // has default value
                            String defaultValue = var.defaultValue.substring(var.defaultValue.lastIndexOf('.') + 1);
                            for (Map<String, Object> enumVars : (List<Map<String, Object>>) var.getAllowableValues().get("enumVars")) {
                                if (defaultValue.equals(enumVars.get("name"))) {
                                    // update default to use the string directly instead of enum string
                                    var.defaultValue = (String) enumVars.get("value");
                                }
                            }
                        }

                        // add import for Set, HashSet
                        cm.imports.add("Set");
                        Map<String, String> importsSet = new HashMap<String, String>();
                        importsSet.put("import", "java.util.Set");
                        imports.add(importsSet);
                        Map<String, String> importsHashSet = new HashMap<String, String>();
                        importsHashSet.put("import", "java.util.HashSet");
                        imports.add(importsHashSet);
                    }

                }

                if (addImports) {
                    Map<String, String> imports2Classnames = new HashMap<>();
                    imports2Classnames.put("JsonNullable", "org.openapitools.jackson.nullable.JsonNullable");
                    imports2Classnames.put("NoSuchElementException", "java.util.NoSuchElementException");
                    imports2Classnames.put("JsonIgnore", "com.fasterxml.jackson.annotation.JsonIgnore");
                    for (Map.Entry<String, String> entry : imports2Classnames.entrySet()) {
                        cm.imports.add(entry.getKey());
                        Map<String, String> importsItem = new HashMap<String, String>();
                        importsItem.put("import", entry.getValue());
                        imports.add(importsItem);
                    }
                }
            }
        }

        // add implements for serializable/parcelable to all models
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            cm.getVendorExtensions().putIfAbsent("x-implements", new ArrayList<String>());
            if (JERSEY2.equals(getLibrary()) || NATIVE.equals(getLibrary()) || OKHTTP_GSON.equals(getLibrary())) {
                cm.getVendorExtensions().put("x-implements", new ArrayList<String>());

                if (cm.oneOf != null && !cm.oneOf.isEmpty() && cm.oneOf.contains("ModelNull")) {
                    // if oneOf contains "null" type
                    cm.isNullable = true;
                    cm.oneOf.remove("ModelNull");
                }

                if (cm.anyOf != null && !cm.anyOf.isEmpty() && cm.anyOf.contains("ModelNull")) {
                    // if anyOf contains "null" type
                    cm.isNullable = true;
                    cm.anyOf.remove("ModelNull");
                }
            }
            if (this.parcelableModel) {
                ((ArrayList<String>) cm.getVendorExtensions().get("x-implements")).add("Parcelable");
            }
        }

        return objs;
    }

    public void setUseOneOfDiscriminatorLookup(boolean useOneOfDiscriminatorLookup) {
        this.useOneOfDiscriminatorLookup = useOneOfDiscriminatorLookup;
    }

    public boolean getUseOneOfDiscriminatorLookup() {
        return this.useOneOfDiscriminatorLookup;
    }

    public void setUseRxJava(boolean useRxJava) {
        this.useRxJava = useRxJava;
        doNotUseRx = false;
    }

    public void setUseRxJava2(boolean useRxJava2) {
        this.useRxJava2 = useRxJava2;
        doNotUseRx = false;
    }

    public void setUseRxJava3(boolean useRxJava3) {
        this.useRxJava3 = useRxJava3;
        doNotUseRx = false;
    }

    public void setDoNotUseRx(boolean doNotUseRx) {
        this.doNotUseRx = doNotUseRx;
    }

    public void setUsePlayWS(boolean usePlayWS) {
        this.usePlayWS = usePlayWS;
    }

    public void setAsyncNative(boolean asyncNative) {
        this.asyncNative = asyncNative;
    }

    public void setMicroprofileFramework(String microprofileFramework) {
        this.microprofileFramework = microprofileFramework;
    }

    public void setConfigKey(String configKey) {
        this.configKey = configKey;
    }

    public void setParcelableModel(boolean parcelableModel) {
        this.parcelableModel = parcelableModel;
    }

    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    public void setPerformBeanValidation(boolean performBeanValidation) {
        this.performBeanValidation = performBeanValidation;
    }

    public void setUseGzipFeature(boolean useGzipFeature) {
        this.useGzipFeature = useGzipFeature;
    }

    public void setUseRuntimeException(boolean useRuntimeException) {
        this.useRuntimeException = useRuntimeException;
    }

    public void setUseReflectionEqualsHashCode(boolean useReflectionEqualsHashCode) {
        this.useReflectionEqualsHashCode = useReflectionEqualsHashCode;
    }

    public void setCaseInsensitiveResponseHeaders(final Boolean caseInsensitiveResponseHeaders) {
        this.caseInsensitiveResponseHeaders = caseInsensitiveResponseHeaders;
    }

    public void setUseAbstractionForFiles(boolean useAbstractionForFiles) {
        this.useAbstractionForFiles = useAbstractionForFiles;
    }

    public void setDynamicOperations(final boolean dynamicOperations) {
        this.dynamicOperations = dynamicOperations;
    }

    public void setSupportStreaming(final boolean supportStreaming) {
        this.supportStreaming = supportStreaming;
    }

    public void setGradleProperties(final String gradleProperties) {
        this.gradleProperties = gradleProperties;
    }

    public void setErrorObjectType(final String errorObjectType) {
        this.errorObjectType = errorObjectType;
    }

    public void setErrorObjectSubtype(final List<String> errorObjectSubtype) {
        this.errorObjectSubtype = errorObjectSubtype;
    }

    /**
     * Serialization library.
     *
     * @return 'gson' or 'jackson'
     */
    public String getSerializationLibrary() {
        return serializationLibrary;
    }

    public void setSerializationLibrary(String serializationLibrary) {
        if (SERIALIZATION_LIBRARY_JACKSON.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_JACKSON;
        } else if (SERIALIZATION_LIBRARY_GSON.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_GSON;
        } else if (SERIALIZATION_LIBRARY_JSONB.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_JSONB;
        } else {
            throw new IllegalArgumentException("Unexpected serializationLibrary value: " + serializationLibrary);
        }
    }

    public void forceSerializationLibrary(String serializationLibrary) {
        if ((this.serializationLibrary != null) && !this.serializationLibrary.equalsIgnoreCase(serializationLibrary)) {
            LOGGER.warn(
                    "The configured serializationLibrary '{}', is not supported by the library: '{}', switching back to: {}",
                    this.serializationLibrary, getLibrary(), serializationLibrary);
        }
        setSerializationLibrary(serializationLibrary);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String toApiVarName(String name) {
        String apiVarName = super.toApiVarName(name);
        if (reservedWords.contains(apiVarName)) {
            apiVarName = escapeReservedWord(apiVarName);
        }
        return apiVarName;
    }

    @Override
    public void addImportsToOneOfInterface(List<Map<String, String>> imports) {
        for (String i : Arrays.asList("JsonSubTypes", "JsonTypeInfo", "JsonIgnoreProperties")) {
            Map<String, String> oneImport = new HashMap<>();
            oneImport.put("import", importMapping.get(i));
            if (!imports.contains(oneImport)) {
                imports.add(oneImport);
            }
        }
    }

    @Override
    public List<VendorExtension> getSupportedVendorExtensions() {
        List<VendorExtension> extensions = super.getSupportedVendorExtensions();
        extensions.add(VendorExtension.X_WEBCLIENT_BLOCKING);
        return extensions;
    }
}
