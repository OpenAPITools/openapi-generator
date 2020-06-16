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
import java.util.regex.Pattern;

import static com.google.common.base.CaseFormat.LOWER_CAMEL;
import static com.google.common.base.CaseFormat.UPPER_UNDERSCORE;
import static java.util.Collections.sort;
import static org.openapitools.codegen.utils.OnceLogger.once;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class JavaClientCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures, GzipFeatures {

    static final String MEDIA_TYPE = "mediaType";

    private static final Logger LOGGER = LoggerFactory.getLogger(JavaClientCodegen.class);

    public static final String USE_RX_JAVA = "useRxJava";
    public static final String USE_RX_JAVA2 = "useRxJava2";
    public static final String USE_RX_JAVA3 = "useRxJava3";
    public static final String DO_NOT_USE_RX = "doNotUseRx";
    public static final String USE_PLAY_WS = "usePlayWS";
    public static final String PLAY_VERSION = "playVersion";
    public static final String FEIGN_VERSION = "feignVersion";
    public static final String ASYNC_NATIVE = "asyncNative";
    public static final String PARCELABLE_MODEL = "parcelableModel";
    public static final String USE_RUNTIME_EXCEPTION = "useRuntimeException";
    public static final String USE_REFLECTION_EQUALS_HASHCODE = "useReflectionEqualsHashCode";
    public static final String CASE_INSENSITIVE_RESPONSE_HEADERS = "caseInsensitiveResponseHeaders";

    public static final String PLAY_24 = "play24";
    public static final String PLAY_25 = "play25";
    public static final String PLAY_26 = "play26";

    public static final String FEIGN_9 = "9.x";
    public static final String FEIGN_10 = "10.x";

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

    public static final String SERIALIZATION_LIBRARY_GSON = "gson";
    public static final String SERIALIZATION_LIBRARY_JACKSON = "jackson";

    protected String gradleWrapperPackage = "gradle.wrapper";
    protected boolean useRxJava = false;
    protected boolean useRxJava2 = false;
    protected boolean useRxJava3 = false;
    // backwards compatibility for openapi configs that specify neither rx1 nor rx2
    // (mustache does not allow for boolean operators so we need this extra field)
    protected boolean doNotUseRx = true;
    protected boolean usePlayWS = false;
    protected String playVersion = PLAY_26;
    protected String feignVersion = FEIGN_10;
    protected boolean asyncNative = false;
    protected boolean parcelableModel = false;
    protected boolean useBeanValidation = false;
    protected boolean performBeanValidation = false;
    protected boolean useGzipFeature = false;
    protected boolean useRuntimeException = false;
    protected boolean useReflectionEqualsHashCode = false;
    protected boolean caseInsensitiveResponseHeaders = false;
    protected String authFolder;
    protected String serializationLibrary = null;

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

        cliOptions.add(CliOption.newBoolean(USE_RX_JAVA, "Whether to use the RxJava adapter with the retrofit2 library. IMPORTANT: this option has been deprecated and will be removed in the 5.x release."));
        cliOptions.add(CliOption.newBoolean(USE_RX_JAVA2, "Whether to use the RxJava2 adapter with the retrofit2 library."));
        cliOptions.add(CliOption.newBoolean(USE_RX_JAVA3, "Whether to use the RxJava3 adapter with the retrofit2 library."));
        cliOptions.add(CliOption.newBoolean(PARCELABLE_MODEL, "Whether to generate models for Android that implement Parcelable with the okhttp-gson library."));
        cliOptions.add(CliOption.newBoolean(USE_PLAY_WS, "Use Play! Async HTTP client (Play WS API)"));
        cliOptions.add(CliOption.newString(PLAY_VERSION, "Version of Play! Framework (possible values \"play24\" (Deprecated), \"play25\" (Deprecated), \"play26\" (Default))"));
        cliOptions.add(CliOption.newBoolean(SUPPORT_JAVA6, "Whether to support Java6 with the Jersey1 library. This option has been deprecated and will be removed in the 5.x release"));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));
        cliOptions.add(CliOption.newBoolean(PERFORM_BEANVALIDATION, "Perform BeanValidation"));
        cliOptions.add(CliOption.newBoolean(USE_GZIP_FEATURE, "Send gzip-encoded requests"));
        cliOptions.add(CliOption.newBoolean(USE_RUNTIME_EXCEPTION, "Use RuntimeException instead of Exception"));
        cliOptions.add(CliOption.newBoolean(ASYNC_NATIVE, "If true, async handlers will be used, instead of the sync version"));
        cliOptions.add(CliOption.newBoolean(FEIGN_VERSION, "Version of OpenFeign: '10.x' (default), '9.x' (deprecated)"));
        cliOptions.add(CliOption.newBoolean(USE_REFLECTION_EQUALS_HASHCODE, "Use org.apache.commons.lang3.builder for equals and hashCode in the models. WARNING: This will fail under a security manager, unless the appropriate permissions are set up correctly and also there's potential performance impact."));
        cliOptions.add(CliOption.newBoolean(CASE_INSENSITIVE_RESPONSE_HEADERS, "Make API response's headers case-insensitive. Available on " + OKHTTP_GSON + ", " + JERSEY2 + " libraries"));

        supportedLibraries.put(JERSEY1, "HTTP client: Jersey client 1.19.x. JSON processing: Jackson 2.9.x. Enable Java6 support using '-DsupportJava6=true'. Enable gzip request encoding using '-DuseGzipFeature=true'. IMPORTANT NOTE: jersey 1.x is no longer actively maintained so please upgrade to 'jersey2' or other HTTP libaries instead.");
        supportedLibraries.put(JERSEY2, "HTTP client: Jersey client 2.25.1. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(FEIGN, "HTTP client: OpenFeign 9.x (deprecated) or 10.x (default). JSON processing: Jackson 2.9.x.");
        supportedLibraries.put(OKHTTP_GSON, "[DEFAULT] HTTP client: OkHttp 3.x. JSON processing: Gson 2.8.x. Enable Parcelable models on Android using '-DparcelableModel=true'. Enable gzip request encoding using '-DuseGzipFeature=true'.");
        supportedLibraries.put(RETROFIT_2, "HTTP client: OkHttp 3.x. JSON processing: Gson 2.x (Retrofit 2.3.0). Enable the RxJava adapter using '-DuseRxJava[2/3]=true'. (RxJava 1.x or 2.x or 3.x)");
        supportedLibraries.put(RESTTEMPLATE, "HTTP client: Spring RestTemplate 4.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(WEBCLIENT, "HTTP client: Spring WebClient 5.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(RESTEASY, "HTTP client: Resteasy client 3.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(VERTX, "HTTP client: VertX client 3.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(GOOGLE_API_CLIENT, "HTTP client: Google API client 1.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(REST_ASSURED, "HTTP client: rest-assured : 4.x. JSON processing: Gson 2.x or Jackson 2.10.x. Only for Java 8");
        supportedLibraries.put(NATIVE, "HTTP client: Java native HttpClient. JSON processing: Jackson 2.9.x. Only for Java11+");
        supportedLibraries.put(MICROPROFILE, "HTTP client: Microprofile client 1.x. JSON processing: Jackson 2.9.x");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        // set okhttp-gson as the default
        libraryOption.setDefault(OKHTTP_GSON);
        cliOptions.add(libraryOption);
        setLibrary(OKHTTP_GSON);

        CliOption serializationLibrary = new CliOption(CodegenConstants.SERIALIZATION_LIBRARY, "Serialization library, default depends from the library");
        Map<String, String> serializationOptions = new HashMap<>();
        serializationOptions.put(SERIALIZATION_LIBRARY_GSON, "Use Gson as serialization library");
        serializationOptions.put(SERIALIZATION_LIBRARY_JACKSON, "Use Jackson as serialization library");
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
        if ((WEBCLIENT.equals(getLibrary()) && "threetenbp".equals(dateLibrary)) || NATIVE.equals(getLibrary())) {
            dateLibrary = "java8";
        } else if (MICROPROFILE.equals(getLibrary())) {
            dateLibrary = "legacy";
        }

        super.processOpts();

        // RxJava
        if (additionalProperties.containsKey(USE_RX_JAVA) && additionalProperties.containsKey(USE_RX_JAVA2) && additionalProperties.containsKey(USE_RX_JAVA3)){
            LOGGER.warn("You specified all RxJava versions 1, 2 and 3 but they are mutually exclusive. Defaulting to v3.");
            this.setUseRxJava3(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA3).toString()));
        }else {
            if (additionalProperties.containsKey(USE_RX_JAVA) && additionalProperties.containsKey(USE_RX_JAVA2)){
                LOGGER.warn("You specified both RxJava versions 1 and 2 but they are mutually exclusive. Defaulting to v2.");
                this.setUseRxJava2(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA2).toString()));
            }else if (additionalProperties.containsKey(USE_RX_JAVA) && additionalProperties.containsKey(USE_RX_JAVA3)) {
                LOGGER.warn("You specified both RxJava versions 1 and 3 but they are mutually exclusive. Defaulting to v3.");
                this.setUseRxJava3(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA3).toString()));
            }else if(additionalProperties.containsKey(USE_RX_JAVA2) && additionalProperties.containsKey(USE_RX_JAVA3)){
                LOGGER.warn("You specified both RxJava versions 2 and 3 but they are mutually exclusive. Defaulting to v3.");
                this.setUseRxJava3(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA3).toString()));
            }else{
                if(additionalProperties.containsKey(USE_RX_JAVA)){
                    this.setUseRxJava(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA).toString()));
                }
                if(additionalProperties.containsKey(USE_RX_JAVA2)){
                    this.setUseRxJava2(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA2).toString()));
                }
                if(additionalProperties.containsKey(USE_RX_JAVA3)){
                    this.setUseRxJava3(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA3).toString()));
                }
            }
        }

        if (!useRxJava && !useRxJava2 && !useRxJava3) {
            additionalProperties.put(DO_NOT_USE_RX, true);
        }

        // Java Play
        if (additionalProperties.containsKey(USE_PLAY_WS)) {
            this.setUsePlayWS(Boolean.valueOf(additionalProperties.get(USE_PLAY_WS).toString()));
        }
        additionalProperties.put(USE_PLAY_WS, usePlayWS);

        if (additionalProperties.containsKey(PLAY_VERSION)) {
            this.setPlayVersion(additionalProperties.get(PLAY_VERSION).toString());
        }
        additionalProperties.put(PLAY_VERSION, playVersion);

        // default to feign 10.x
        if (additionalProperties.containsKey(FEIGN_VERSION)) {
            once(LOGGER).warn("feignVersion has been deprecated. 10.x is the default.");
        }
        additionalProperties.put(FEIGN_VERSION, feignVersion);

        if (additionalProperties.containsKey(ASYNC_NATIVE)) {
            this.setAsyncNative(convertPropertyToBooleanAndWriteBack(ASYNC_NATIVE));
        }

        if (additionalProperties.containsKey(PARCELABLE_MODEL)) {
            this.setParcelableModel(Boolean.valueOf(additionalProperties.get(PARCELABLE_MODEL).toString()));
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

        final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");
        final String apiFolder = (sourceFolder + '/' + apiPackage).replace(".", "/");
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
        supportingFiles.add(new SupportingFile("openapi.mustache", "api", "openapi.yaml"));


        if (!(RESTTEMPLATE.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || NATIVE.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) {
            supportingFiles.add(new SupportingFile("StringUtil.mustache", invokerFolder, "StringUtil.java"));
        }

        // google-api-client doesn't use the OpenAPI auth, because it uses Google Credential directly (HttpRequestInitializer)
        if (!(GOOGLE_API_CLIENT.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || NATIVE.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) {
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
        } else if (OKHTTP_GSON.equals(getLibrary()) || StringUtils.isEmpty(getLibrary())) {
            // the "okhttp-gson" library template requires "ApiCallback.mustache" for async call
            supportingFiles.add(new SupportingFile("ApiCallback.mustache", invokerFolder, "ApiCallback.java"));
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("ProgressRequestBody.mustache", invokerFolder, "ProgressRequestBody.java"));
            supportingFiles.add(new SupportingFile("ProgressResponseBody.mustache", invokerFolder, "ProgressResponseBody.java"));
            supportingFiles.add(new SupportingFile("GzipRequestInterceptor.mustache", invokerFolder, "GzipRequestInterceptor.java"));

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
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            if (ProcessUtils.hasHttpSignatureMethods(openAPI)) {
                supportingFiles.add(new SupportingFile("auth/HttpSignatureAuth.mustache", authFolder, "HttpSignatureAuth.java"));
            }
            supportingFiles.add(new SupportingFile("AbstractOpenApiSchema.mustache", (sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar), "AbstractOpenApiSchema.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);

            // Composed schemas can have the 'additionalProperties' keyword, as specified in JSON schema.
            // In principle, this should be enabled by default for all code generators. However due to limitations
            // in other code generators, support needs to be enabled on a case-by-case basis.
            // The flag below should be set for all Java libraries, but the templates need to be ported
            // one by one for each library.
            supportsAdditionalPropertiesWithComposedSchema = true;

        } else if (NATIVE.equals(getLibrary())) {
            setJava8Mode(true);
            additionalProperties.put("java8", "true");
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
            setJava8Mode(true);
            additionalProperties.put("java8", "true");
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        } else if (VERTX.equals(getLibrary())) {
            typeMapping.put("file", "AsyncFile");
            importMapping.put("AsyncFile", "io.vertx.core.file.AsyncFile");
            setJava8Mode(true);
            additionalProperties.put("java8", "true");
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
            apiTemplateFiles.put("apiImpl.mustache", "Impl.java");
            apiTemplateFiles.put("rxApiImpl.mustache", ".java");
            supportingFiles.remove(new SupportingFile("manifest.mustache", projectFolder, "AndroidManifest.xml"));
        } else if (GOOGLE_API_CLIENT.equals(getLibrary())) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);

        } else if (REST_ASSURED.equals(getLibrary())) {
            if (getSerializationLibrary() == null) {
                LOGGER.info("No serializationLibrary configured, using '" + SERIALIZATION_LIBRARY_GSON + "' as fallback");
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
            importMapping.put("LocalDate", "org.joda.time.LocalDate");
            serializationLibrary = "none";
        } else {
            LOGGER.error("Unknown library option (-l/--library): " + getLibrary());
        }

        if (usePlayWS) {
            // remove unsupported auth
            Iterator<SupportingFile> iter = supportingFiles.iterator();
            while (iter.hasNext()) {
                SupportingFile sf = iter.next();
                if (sf.templateFile.startsWith("auth/")) {
                    iter.remove();
                }
            }

            apiTemplateFiles.remove("api.mustache");

            if (PLAY_24.equals(playVersion)) {
                LOGGER.warn("`play24` option has been deprecated and will be removed in the 5.x release. Please use `play26` instead.");
                additionalProperties.put(PLAY_24, true);
                apiTemplateFiles.put("play24/api.mustache", ".java");

                supportingFiles.add(new SupportingFile("play24/ApiClient.mustache", invokerFolder, "ApiClient.java"));
                supportingFiles.add(new SupportingFile("play24/Play24CallFactory.mustache", invokerFolder, "Play24CallFactory.java"));
                supportingFiles.add(new SupportingFile("play24/Play24CallAdapterFactory.mustache", invokerFolder,
                        "Play24CallAdapterFactory.java"));
            }

            if (PLAY_25.equals(playVersion)) {
                LOGGER.warn("`play25` option has been deprecated and will be removed in the 5.x release. Please use `play26` instead.");
                additionalProperties.put(PLAY_25, true);
                apiTemplateFiles.put("play25/api.mustache", ".java");

                supportingFiles.add(new SupportingFile("play25/ApiClient.mustache", invokerFolder, "ApiClient.java"));
                supportingFiles.add(new SupportingFile("play25/Play25CallFactory.mustache", invokerFolder, "Play25CallFactory.java"));
                supportingFiles.add(new SupportingFile("play25/Play25CallAdapterFactory.mustache", invokerFolder,
                        "Play25CallAdapterFactory.java"));
                additionalProperties.put("java8", "true");
            }

            if (PLAY_26.equals(playVersion)) {
                additionalProperties.put(PLAY_26, true);
                apiTemplateFiles.put("play26/api.mustache", ".java");

                supportingFiles.add(new SupportingFile("play26/ApiClient.mustache", invokerFolder, "ApiClient.java"));
                supportingFiles.add(new SupportingFile("play26/Play26CallFactory.mustache", invokerFolder, "Play26CallFactory.java"));
                supportingFiles.add(new SupportingFile("play26/Play26CallAdapterFactory.mustache", invokerFolder,
                        "Play26CallAdapterFactory.java"));
                additionalProperties.put("java8", "true");
            }

            supportingFiles.add(new SupportingFile("play-common/auth/ApiKeyAuth.mustache", authFolder, "ApiKeyAuth.java"));
            supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));
            supportingFiles.add(new SupportingFile("Pair.mustache", invokerFolder, "Pair.java"));

            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        }

        if (getSerializationLibrary() == null) {
            LOGGER.info("No serializationLibrary configured, using '" + SERIALIZATION_LIBRARY_GSON + "' as fallback");
            setSerializationLibrary(SERIALIZATION_LIBRARY_GSON);
        }
        if (SERIALIZATION_LIBRARY_JACKSON.equals(getSerializationLibrary())) {
            additionalProperties.put(SERIALIZATION_LIBRARY_JACKSON, "true");
            additionalProperties.remove(SERIALIZATION_LIBRARY_GSON);
            if (!NATIVE.equals(getLibrary())) {
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache", invokerFolder, "RFC3339DateFormat.java"));
                if ("threetenbp".equals(dateLibrary) && !usePlayWS) {
                    supportingFiles.add(new SupportingFile("CustomInstantDeserializer.mustache", invokerFolder, "CustomInstantDeserializer.java"));
                }
            }
        } else if (SERIALIZATION_LIBRARY_GSON.equals(getSerializationLibrary())) {
            additionalProperties.put(SERIALIZATION_LIBRARY_GSON, "true");
            additionalProperties.remove(SERIALIZATION_LIBRARY_JACKSON);
        } else {
            additionalProperties.remove(SERIALIZATION_LIBRARY_JACKSON);
            additionalProperties.remove(SERIALIZATION_LIBRARY_GSON);
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
                        Iterator<CodegenParameter> iterator = operation.allParams.iterator();
                        while (iterator.hasNext()) {
                            CodegenParameter param = iterator.next();
                            param.hasMore = iterator.hasNext();
                        }
                    }
                }
            }

        }

        // camelize path variables for Feign client
        if (FEIGN.equals(getLibrary())) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation op : operationList) {
                String path = op.path;
                String[] items = path.split("/", -1);

                for (int i = 0; i < items.length; ++i) {
                    if (items[i].matches("^\\{(.*)\\}$")) { // wrap in {}
                        // camelize path variable
                        items[i] = "{" + camelize(items[i].substring(1, items[i].length() - 1), true) + "}";
                    }
                }
                op.path = StringUtils.join(items, "/");
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

            consume.put("hasMore", "true");
        }

        prioritizedContentTypes.addAll(0, jsonMimeTypes);
        prioritizedContentTypes.addAll(0, jsonVendorMimeTypes);

        prioritizedContentTypes.get(prioritizedContentTypes.size() - 1).put("hasMore", null);

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
                    boolean isOptionalNullable = Boolean.FALSE.equals(var.required) && Boolean.TRUE.equals(var.isNullable);
                    // only add JsonNullable and related imports to optional and nullable values
                    addImports |= isOptionalNullable;
                    var.getVendorExtensions().put("x-is-jackson-optional-nullable", isOptionalNullable);
                }
                if (addImports) {
                    Map<String, String> imports2Classnames = new HashMap<String, String>() {{
                        put("JsonNullable", "org.openapitools.jackson.nullable.JsonNullable");
                        put("NoSuchElementException", "java.util.NoSuchElementException");
                        put("JsonIgnore", "com.fasterxml.jackson.annotation.JsonIgnore");
                    }};
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
            if (JERSEY2.equals(getLibrary())) {
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
            if (this.serializableModel) {
                ((ArrayList<String>) cm.getVendorExtensions().get("x-implements")).add("Serializable");
            }
        }

        return objs;
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

    public void setPlayVersion(String playVersion) {
        this.playVersion = playVersion;
    }

    public void setFeignVersion(String feignVersion) {
        this.feignVersion = feignVersion;
    }

    public void setAsyncNative(boolean asyncNative) {
        this.asyncNative = asyncNative;
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
        } else {
            throw new IllegalArgumentException("Unexpected serializationLibrary value: " + serializationLibrary);
        }
    }

    public void forceSerializationLibrary(String serializationLibrary) {
        if ((this.serializationLibrary != null) && !this.serializationLibrary.equalsIgnoreCase(serializationLibrary)) {
            LOGGER.warn("The configured serializationLibrary '" + this.serializationLibrary + "', is not supported by the library: '" + getLibrary() + "', switching back to: " + serializationLibrary);
        }
        setSerializationLibrary(serializationLibrary);
    }

    final private static Pattern JSON_MIME_PATTERN = Pattern.compile("(?i)application\\/json(;.*)?");
    final private static Pattern JSON_VENDOR_MIME_PATTERN = Pattern.compile("(?i)application\\/vnd.(.*)+json(;.*)?");

    /**
     * Check if the given MIME is a JSON MIME.
     * JSON MIME examples:
     * application/json
     * application/json; charset=UTF8
     * APPLICATION/JSON
     */
    static boolean isJsonMimeType(String mime) {
        return mime != null && (JSON_MIME_PATTERN.matcher(mime).matches());
    }

    /**
     * Check if the given MIME is a JSON Vendor MIME.
     * JSON MIME examples:
     * application/vnd.mycompany+json
     * application/vnd.mycompany.resourceA.version1+json
     */
    static boolean isJsonVendorMimeType(String mime) {
        return mime != null && JSON_VENDOR_MIME_PATTERN.matcher(mime).matches();
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
        for (String i : Arrays.asList("JsonSubTypes", "JsonTypeInfo")) {
            Map<String, String> oneImport = new HashMap<String, String>() {{
                put("import", importMapping.get(i));
            }};
            if (!imports.contains(oneImport)) {
                imports.add(oneImport);
            }
        }
    }
}
