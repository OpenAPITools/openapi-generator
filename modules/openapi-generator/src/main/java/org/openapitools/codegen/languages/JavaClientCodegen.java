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
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.GzipFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
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
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
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
    public static final String CONFIG_KEY_FROM_CLASS_NAME = "configKeyFromClassName";
    public static final String PARCELABLE_MODEL = "parcelableModel";
    public static final String USE_RUNTIME_EXCEPTION = "useRuntimeException";
    public static final String USE_REFLECTION_EQUALS_HASHCODE = "useReflectionEqualsHashCode";
    public static final String CASE_INSENSITIVE_RESPONSE_HEADERS = "caseInsensitiveResponseHeaders";
    public static final String MICROPROFILE_FRAMEWORK = "microprofileFramework";
    public static final String MICROPROFILE_MUTINY = "microprofileMutiny";
    public static final String USE_ABSTRACTION_FOR_FILES = "useAbstractionForFiles";
    public static final String DYNAMIC_OPERATIONS = "dynamicOperations";
    public static final String SUPPORT_STREAMING = "supportStreaming";
    public static final String SUPPORT_URL_QUERY = "supportUrlQuery";
    public static final String GRADLE_PROPERTIES = "gradleProperties";
    public static final String ERROR_OBJECT_TYPE = "errorObjectType";

    public static final String FEIGN = "feign";
    public static final String FEIGN_HC5 = "feign-hc5";
    public static final String GOOGLE_API_CLIENT = "google-api-client";
    public static final String JERSEY2 = "jersey2";
    public static final String JERSEY3 = "jersey3";
    public static final String NATIVE = "native";
    public static final String OKHTTP_GSON = "okhttp-gson";
    public static final String RESTEASY = "resteasy";
    public static final String RESTTEMPLATE = "resttemplate";
    public static final String WEBCLIENT = "webclient";
    public static final String RESTCLIENT = "restclient";
    public static final String REST_ASSURED = "rest-assured";
    public static final String RETROFIT_2 = "retrofit2";
    public static final String VERTX = "vertx";
    public static final String MICROPROFILE = "microprofile";
    public static final String APACHE = "apache-httpclient";
    public static final String MICROPROFILE_REST_CLIENT_VERSION = "microprofileRestClientVersion";
    public static final String MICROPROFILE_REST_CLIENT_DEFAULT_VERSION = "2.0";
    public static final String MICROPROFILE_REST_CLIENT_DEFAULT_ROOT_PACKAGE = "javax";
    public static final String MICROPROFILE_DEFAULT = "default";
    public static final String MICROPROFILE_KUMULUZEE = "kumuluzee";
    public static final String WEBCLIENT_BLOCKING_OPERATIONS = "webclientBlockingOperations";
    public static final String USE_ENUM_CASE_INSENSITIVE = "useEnumCaseInsensitive";
    public static final String FAIL_ON_UNKNOWN_PROPERTIES = "failOnUnknownProperties";
    public static final String SUPPORT_VERTX_FUTURE = "supportVertxFuture";

    public static final String SERIALIZATION_LIBRARY_GSON = "gson";
    public static final String SERIALIZATION_LIBRARY_JACKSON = "jackson";
    public static final String SERIALIZATION_LIBRARY_JSONB = "jsonb";

    public static final String GENERATE_CLIENT_AS_BEAN = "generateClientAsBean";

    protected String gradleWrapperPackage = "gradle.wrapper";
    protected boolean useRxJava = false;
    protected boolean useRxJava2 = false;
    protected boolean useRxJava3 = false;
    // backwards compatibility for openapi configs that specify neither rx1 nor rx2
    // (mustache does not allow for boolean operators so we need this extra field)
    @Setter protected boolean doNotUseRx = true;
    @Setter protected boolean usePlayWS = false;
    @Setter protected String microprofileFramework = MICROPROFILE_DEFAULT;
    @Setter protected String microprofileRestClientVersion = MICROPROFILE_REST_CLIENT_DEFAULT_VERSION;
    @Setter protected boolean microprofileMutiny = false;
    @Setter protected String configKey = null;
    @Setter(AccessLevel.PRIVATE) protected boolean configKeyFromClassName = false;

    @Setter protected boolean asyncNative = false;
    @Setter protected boolean parcelableModel = false;
    @Setter protected boolean performBeanValidation = false;
    @Setter protected boolean useGzipFeature = false;
    @Setter protected boolean useRuntimeException = false;
    @Setter protected boolean useReflectionEqualsHashCode = false;
    protected boolean caseInsensitiveResponseHeaders = false;
    @Setter protected boolean useAbstractionForFiles = false;
    @Setter protected boolean dynamicOperations = false;
    @Setter protected boolean supportStreaming = false;
    @Setter protected boolean withAWSV4Signature = false;
    @Setter protected String gradleProperties;
    @Setter protected String errorObjectType;
    @Getter @Setter protected boolean failOnUnknownProperties = false;
    protected String authFolder;
    /**
     * Serialization library.
     */
    @Getter protected String serializationLibrary = null;
    @Setter protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup
    protected String rootJavaEEPackage;
    protected Map<String, MpRestClientVersion> mpRestClientVersions = new LinkedHashMap<>();
    @Setter(AccessLevel.PRIVATE) protected String useSingleRequestParameter = "false";
    protected boolean webclientBlockingOperations = false;
    @Setter protected boolean generateClientAsBean = false;
    @Setter protected boolean useEnumCaseInsensitive = false;

    @Setter protected int maxAttemptsForRetry = 1;
    @Setter protected long waitTimeMillis = 10l;

    private static class MpRestClientVersion {
        public final String rootPackage;
        public final String pomTemplate;

        public MpRestClientVersion(String rootPackage, String pomTemplate) {
            this.rootPackage = rootPackage;
            this.pomTemplate = pomTemplate;
        }
    }

    @Override
    public DocumentationProvider defaultDocumentationProvider() {
        return DocumentationProvider.SOURCE;
    }

    @Override
    public List<DocumentationProvider> supportedDocumentationProvider() {
        List<DocumentationProvider> documentationProviders = new ArrayList<>();
        documentationProviders.add(DocumentationProvider.NONE);
        documentationProviders.add(DocumentationProvider.SOURCE);
        return documentationProviders;
    }

    @Override
    public List<AnnotationLibrary> supportedAnnotationLibraries() {
        List<AnnotationLibrary> annotationLibraries = new ArrayList<>();
        annotationLibraries.add(AnnotationLibrary.NONE);
        annotationLibraries.add(AnnotationLibrary.SWAGGER1);
        annotationLibraries.add(AnnotationLibrary.SWAGGER2);
        return annotationLibraries;
    }

    public JavaClientCodegen() {
        super();

        // TODO: Move GlobalFeature.ParameterizedServer to library: jersey after moving featureSet to generatorMetadata
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .includeGlobalFeatures(GlobalFeature.ParameterizedServer)
                .includeSecurityFeatures(SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_ClientCredentials,
                        SecurityFeature.OAuth2_Password,
                        SecurityFeature.SignatureAuth,//jersey only
                        SecurityFeature.AWSV4Signature)//okhttp-gson only
        );

        outputFolder = "generated-code" + File.separator + "java";
        embeddedTemplateDir = templateDir = "Java";
        invokerPackage = "org.openapitools.client";
        artifactId = "openapi-java-client";
        apiPackage = "org.openapitools.client.api";
        modelPackage = "org.openapitools.client.model";
        rootJavaEEPackage = MICROPROFILE_REST_CLIENT_DEFAULT_ROOT_PACKAGE;

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
        cliOptions.add(CliOption.newBoolean(USE_RUNTIME_EXCEPTION, "Use RuntimeException instead of Exception. Only jersey2, jersey3, okhttp-gson, vertx, microprofile support this option."));
        cliOptions.add(CliOption.newBoolean(ASYNC_NATIVE, "If true, async handlers will be used, instead of the sync version"));
        cliOptions.add(CliOption.newBoolean(USE_REFLECTION_EQUALS_HASHCODE, "Use org.apache.commons.lang3.builder for equals and hashCode in the models. WARNING: This will fail under a security manager, unless the appropriate permissions are set up correctly and also there's potential performance impact."));
        cliOptions.add(CliOption.newBoolean(CASE_INSENSITIVE_RESPONSE_HEADERS, "Make API response's headers case-insensitive. Available on " + OKHTTP_GSON + ", " + JERSEY2 + " libraries"));
        cliOptions.add(CliOption.newString(MICROPROFILE_FRAMEWORK, "Framework for microprofile. Possible values \"kumuluzee\""));
        cliOptions.add(CliOption.newString(MICROPROFILE_MUTINY, "Whether to use async types for microprofile (currently only Smallrye Mutiny is supported)."));
        cliOptions.add(CliOption.newBoolean(USE_ABSTRACTION_FOR_FILES, "Use alternative types instead of java.io.File to allow passing bytes without a file on disk. Available on resttemplate, webclient, restclient, libraries"));
        cliOptions.add(CliOption.newBoolean(DYNAMIC_OPERATIONS, "Generate operations dynamically at runtime from an OAS", this.dynamicOperations));
        cliOptions.add(CliOption.newBoolean(SUPPORT_STREAMING, "Support streaming endpoint (beta)", this.supportStreaming));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT, CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT_DESC + " (only available for okhttp-gson library)", this.withAWSV4Signature));
        cliOptions.add(CliOption.newString(GRADLE_PROPERTIES, "Append additional Gradle properties to the gradle.properties file"));
        cliOptions.add(CliOption.newString(ERROR_OBJECT_TYPE, "Error Object type. (This option is for okhttp-gson only)"));
        cliOptions.add(CliOption.newString(CONFIG_KEY, "Config key in @RegisterRestClient. Default to none. Only `microprofile` supports this option."));
        cliOptions.add(CliOption.newString(CONFIG_KEY_FROM_CLASS_NAME, "If true, set tag as key in @RegisterRestClient. Default to false. Only `microprofile` supports this option."));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP_DESC + " Only jersey2, jersey3, native, okhttp-gson support this option."));
        cliOptions.add(CliOption.newString(MICROPROFILE_REST_CLIENT_VERSION, "Version of MicroProfile Rest Client API."));
        cliOptions.add(CliOption.newString(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, "Setting this property to \"true\" will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter. ONLY jersey2, jersey3, okhttp-gson, microprofile, Spring RestClient, Spring WebClient support this option. Setting this property to \"static\" does the same as \"true\", but also makes the generated arguments class static with single parameter instantiation.").defaultValue("false"));
        cliOptions.add(CliOption.newBoolean(WEBCLIENT_BLOCKING_OPERATIONS, "Making all WebClient operations blocking(sync). Note that if on operation 'x-webclient-blocking: false' then such operation won't be sync", this.webclientBlockingOperations));
        cliOptions.add(CliOption.newBoolean(GENERATE_CLIENT_AS_BEAN, "For resttemplate, restclient and webclient, configure whether to create `ApiClient.java` and Apis clients as bean (with `@Component` annotation).", this.generateClientAsBean));
        cliOptions.add(CliOption.newBoolean(SUPPORT_URL_QUERY, "Generate toUrlQueryString in POJO (default to true). Available on `native`, `apache-httpclient` libraries."));
        cliOptions.add(CliOption.newBoolean(USE_ENUM_CASE_INSENSITIVE, "Use `equalsIgnoreCase` when String for enum comparison", useEnumCaseInsensitive));
        cliOptions.add(CliOption.newBoolean(FAIL_ON_UNKNOWN_PROPERTIES, "Fail Jackson de-serialization on unknown properties", this.failOnUnknownProperties));
        cliOptions.add(CliOption.newBoolean(SUPPORT_VERTX_FUTURE, "Also generate api methods that return a vertx Future instead of taking a callback. Only `vertx` supports this option. Requires vertx 4 or greater."));

        supportedLibraries.put(JERSEY2, "HTTP client: Jersey client 2.25.1. JSON processing: Jackson 2.17.1");
        supportedLibraries.put(JERSEY3, "HTTP client: Jersey client 3.1.1. JSON processing: Jackson 2.17.1");
        supportedLibraries.put(FEIGN, "HTTP client: OpenFeign 13.2.1. JSON processing: Jackson 2.17.1 or Gson 2.10.1");
        supportedLibraries.put(FEIGN_HC5, "HTTP client: OpenFeign 13.2.1/HttpClient5 5.4.2. JSON processing: Jackson 2.17.1 or Gson 2.10.1");
        supportedLibraries.put(OKHTTP_GSON, "[DEFAULT] HTTP client: OkHttp 4.11.0. JSON processing: Gson 2.10.1. Enable Parcelable models on Android using '-DparcelableModel=true'. Enable gzip request encoding using '-DuseGzipFeature=true'.");
        supportedLibraries.put(RETROFIT_2, "HTTP client: OkHttp 4.11.0. JSON processing: Gson 2.10.1 (Retrofit 2.5.0) or Jackson 2.17.1. Enable the RxJava adapter using '-DuseRxJava[2/3]=true'. (RxJava 1.x or 2.x or 3.x)");
        supportedLibraries.put(RESTTEMPLATE, "HTTP client: Spring RestTemplate 5.3.33 (6.1.5 if `useJakartaEe=true`). JSON processing: Jackson 2.17.1");
        supportedLibraries.put(WEBCLIENT, "HTTP client: Spring WebClient 5.1.18. JSON processing: Jackson 2.17.1");
        supportedLibraries.put(RESTCLIENT, "HTTP client: Spring RestClient 6.1.6. JSON processing: Jackson 2.17.1");
        supportedLibraries.put(RESTEASY, "HTTP client: Resteasy client 4.7.6. JSON processing: Jackson 2.17.1");
        supportedLibraries.put(VERTX, "HTTP client: VertX client 3.5.2. JSON processing: Jackson 2.17.1");
        supportedLibraries.put(GOOGLE_API_CLIENT, "HTTP client: Google API client 2.2.0. JSON processing: Jackson 2.17.1");
        supportedLibraries.put(REST_ASSURED, "HTTP client: rest-assured 5.3.2. JSON processing: Gson 2.10.1 or Jackson 2.17.1. Only for Java 8");
        supportedLibraries.put(NATIVE, "HTTP client: Java native HttpClient. JSON processing: Jackson 2.17.1. Only for Java11+");
        supportedLibraries.put(MICROPROFILE, "HTTP client: Microprofile client " + MICROPROFILE_REST_CLIENT_DEFAULT_VERSION + " (default, set desired version via `" + MICROPROFILE_REST_CLIENT_VERSION + "=x.x.x`). JSON processing: JSON-B 1.0.2 or Jackson 2.17.1");
        supportedLibraries.put(APACHE, "HTTP client: Apache httpclient 5.2.1. JSON processing: Jackson 2.17.1");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        // set okhttp-gson as the default
        libraryOption.setDefault(OKHTTP_GSON);
        cliOptions.add(libraryOption);
        setLibrary(OKHTTP_GSON);

        CliOption serializationLibrary = new CliOption(CodegenConstants.SERIALIZATION_LIBRARY,
                "Serialization library, default depends on value of the option library");
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

        initMpRestClientVersionToRootPackage();
    }

    private void initMpRestClientVersionToRootPackage() {
        mpRestClientVersions.put("1.4.1", new MpRestClientVersion("javax", "pom.mustache"));
        mpRestClientVersions.put("2.0", new MpRestClientVersion("javax", "pom.mustache"));
        mpRestClientVersions.put("3.0", new MpRestClientVersion("jakarta", "pom_3.0.mustache"));
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
        if (isLibrary(MICROPROFILE)) {
            co.subresourceOperation = !co.path.isEmpty();
        }
    }

    @Override
    public void processOpts() {
        // this is before super.processOpts() because that method uses dateLibrary to select imports
        if (isLibrary(WEBCLIENT) || isLibrary(NATIVE) || isLibrary(RESTCLIENT)) {
            dateLibrary = "java8";
        } else if (isLibrary(MICROPROFILE)) {
            dateLibrary = "legacy";
        }

        super.processOpts();    // can actually change the library (possibly only in unit-tests but still...)

        // determine and cache client library type once
        final boolean libApache = isLibrary(APACHE);
        final boolean libFeign = isLibrary(FEIGN) || isLibrary(FEIGN_HC5);
        final boolean libGoogleApiClient = isLibrary(GOOGLE_API_CLIENT);
        final boolean libJersey2 = isLibrary(JERSEY2);
        final boolean libJersey3 = isLibrary(JERSEY3);
        final boolean libMicroprofile = isLibrary(MICROPROFILE);
        final boolean libNative = isLibrary(NATIVE);
        final boolean libOkHttpGson = isLibrary(OKHTTP_GSON) || StringUtils.isBlank(getLibrary());
        final boolean libRestAssured = isLibrary(REST_ASSURED);
        final boolean libRestClient = isLibrary(RESTCLIENT);
        final boolean libRestEasy = isLibrary(RESTEASY);
        final boolean libRestTemplate = isLibrary(RESTTEMPLATE);
        final boolean libRetrofit2 = isLibrary(RETROFIT_2);
        final boolean libVertx = isLibrary(VERTX);
        final boolean libWebClient = isLibrary(WEBCLIENT);

        // default jackson unless overridden by setSerializationLibrary
        this.jackson = !additionalProperties.containsKey(CodegenConstants.SERIALIZATION_LIBRARY) ||
                SERIALIZATION_LIBRARY_JACKSON.equals(additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY));

        convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, this::setUseOneOfDiscriminatorLookup);

        // RxJava
        if (additionalProperties.containsKey(USE_RX_JAVA2) && additionalProperties.containsKey(USE_RX_JAVA3)) {
            LOGGER.warn("You specified all RxJava versions 2 and 3 but they are mutually exclusive. Defaulting to v3.");
            convertPropertyToBooleanAndWriteBack(USE_RX_JAVA3, this::setUseRxJava3);
            writePropertyBack(USE_RX_JAVA2, false);
        } else {
            convertPropertyToBooleanAndWriteBack(USE_RX_JAVA3, this::setUseRxJava3);
            convertPropertyToBooleanAndWriteBack(USE_RX_JAVA2, this::setUseRxJava2);
        }
        convertPropertyToStringAndWriteBack(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, this::setUseSingleRequestParameter);
        writePropertyBack("singleRequestParameter", getSingleRequestParameter());
        writePropertyBack("staticRequest", getStaticRequest());

        if (!useRxJava && !useRxJava2 && !useRxJava3) {
            additionalProperties.put(DO_NOT_USE_RX, true);
        }

        // Java Play
        convertPropertyToBooleanAndWriteBack(USE_PLAY_WS, this::setUsePlayWS);

        // Microprofile framework
        if (additionalProperties.containsKey(MICROPROFILE_FRAMEWORK)) {
            if (!MICROPROFILE_KUMULUZEE.equals(microprofileFramework)) {
                throw new RuntimeException("Invalid microprofileFramework '" + microprofileFramework + "'. Must be 'kumuluzee' or none.");
            }
//            this.setMicroprofileFramework(additionalProperties.get(MICROPROFILE_FRAMEWORK).toString());
        }
        convertPropertyToStringAndWriteBack(MICROPROFILE_FRAMEWORK, this::setMicroprofileFramework);

        convertPropertyToBooleanAndWriteBack(MICROPROFILE_MUTINY, this::setMicroprofileMutiny);

        convertPropertyToStringAndWriteBack(MICROPROFILE_REST_CLIENT_VERSION, value -> microprofileRestClientVersion = value);
        if (!mpRestClientVersions.containsKey(microprofileRestClientVersion)) {
            throw new IllegalArgumentException(
                    String.format(Locale.ROOT,
                            "Version %s of MicroProfile Rest Client is not supported or incorrect. Supported versions are %s",
                            microprofileRestClientVersion,
                            String.join(", ", mpRestClientVersions.keySet())
                    )
            );
        }

        if (!additionalProperties.containsKey("rootJavaEEPackage")) {
            String mpRestClientVersion = (String) additionalProperties.get(MICROPROFILE_REST_CLIENT_VERSION);
            if (mpRestClientVersions.containsKey(mpRestClientVersion)) {
                rootJavaEEPackage = mpRestClientVersions.get(mpRestClientVersion).rootPackage;
            }
            additionalProperties.put("rootJavaEEPackage", rootJavaEEPackage);
        }

        if (additionalProperties.containsKey(CONFIG_KEY)) {
            convertPropertyToStringAndWriteBack(CONFIG_KEY, this::setConfigKey);
        } else {
            convertPropertyToBooleanAndWriteBack(CONFIG_KEY_FROM_CLASS_NAME, this::setConfigKeyFromClassName);
        }

        convertPropertyToBooleanAndWriteBack(ASYNC_NATIVE, this::setAsyncNative);
        convertPropertyToBooleanAndWriteBack(PARCELABLE_MODEL, this::setParcelableModel);
        convertPropertyToBooleanAndWriteBack(PERFORM_BEANVALIDATION, this::setPerformBeanValidation);
        convertPropertyToBooleanAndWriteBack(USE_GZIP_FEATURE, this::setUseGzipFeature);
        convertPropertyToBooleanAndWriteBack(USE_RUNTIME_EXCEPTION, this::setUseRuntimeException);
        convertPropertyToBooleanAndWriteBack(USE_REFLECTION_EQUALS_HASHCODE, this::setUseReflectionEqualsHashCode);
        convertPropertyToBooleanAndWriteBack(CASE_INSENSITIVE_RESPONSE_HEADERS, this::setUseReflectionEqualsHashCode);
        convertPropertyToBooleanAndWriteBack(USE_ABSTRACTION_FOR_FILES, this::setUseAbstractionForFiles);
        convertPropertyToBooleanAndWriteBack(DYNAMIC_OPERATIONS, this::setDynamicOperations);
        convertPropertyToBooleanAndWriteBack(SUPPORT_STREAMING, this::setSupportStreaming);
        convertPropertyToBooleanAndWriteBack(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT, this::setWithAWSV4Signature);
        convertPropertyToStringAndWriteBack(GRADLE_PROPERTIES, this::setGradleProperties);
        convertPropertyToStringAndWriteBack(ERROR_OBJECT_TYPE, this::setErrorObjectType);
        convertPropertyToBooleanAndWriteBack(WEBCLIENT_BLOCKING_OPERATIONS, op -> webclientBlockingOperations = op);
        convertPropertyToBooleanAndWriteBack(FAIL_ON_UNKNOWN_PROPERTIES, this::setFailOnUnknownProperties);

        // add URL query deepObject support to native, apache-httpclient by default
        if (!additionalProperties.containsKey(SUPPORT_URL_QUERY)) {
            if (libNative || libApache) {
                // default to true for native and apache-httpclient
                additionalProperties.put(SUPPORT_URL_QUERY, true);
            }
        } else {
            additionalProperties.put(SUPPORT_URL_QUERY, Boolean.parseBoolean(additionalProperties.get(SUPPORT_URL_QUERY).toString()));
        }

        convertPropertyToBooleanAndWriteBack(GENERATE_CLIENT_AS_BEAN, this::setGenerateClientAsBean);
        convertPropertyToBooleanAndWriteBack(USE_ENUM_CASE_INSENSITIVE, this::setUseEnumCaseInsensitive);
        convertPropertyToTypeAndWriteBack(CodegenConstants.MAX_ATTEMPTS_FOR_RETRY, Integer::parseInt, this::setMaxAttemptsForRetry);
        convertPropertyToTypeAndWriteBack(CodegenConstants.WAIT_TIME_OF_THREAD, Long::parseLong, this::setWaitTimeMillis);

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
        if (additionalProperties.containsKey("jsr310") && (libWebClient || libVertx || libRestTemplate || libRestEasy
                || libMicroprofile || libJersey2 || libJersey3 || libApache || libRestClient)) {
            supportingFiles.add(new SupportingFile("JavaTimeFormatter.mustache", invokerFolder, "JavaTimeFormatter.java"));
        }

        if (!(libRestTemplate || libRestAssured || libNative || libMicroprofile)) {
            supportingFiles.add(new SupportingFile("StringUtil.mustache", invokerFolder, "StringUtil.java"));
        }

        // google-api-client doesn't use the OpenAPI auth, because it uses Google Credential directly (HttpRequestInitializer)
        if (!(libGoogleApiClient || libRestAssured || libNative || libMicroprofile)) {
            supportingFiles.add(new SupportingFile("auth/HttpBasicAuth.mustache", authFolder, "HttpBasicAuth.java"));
            supportingFiles.add(new SupportingFile("auth/HttpBearerAuth.mustache", authFolder, "HttpBearerAuth.java"));
            supportingFiles.add(new SupportingFile("auth/ApiKeyAuth.mustache", authFolder, "ApiKeyAuth.java"));
            if (libOkHttpGson && withAWSV4Signature) {
                supportingFiles.add(new SupportingFile("auth/AWS4Auth.mustache", authFolder, "AWS4Auth.java"));
            }
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

        convertPropertyToStringAndWriteBack(CodegenConstants.SERIALIZATION_LIBRARY, this::setSerializationLibrary);

        //TODO: add auto-generated doc to feign
        if (libFeign) {
            modelDocTemplateFiles.remove("model_doc.mustache");
            apiDocTemplateFiles.remove("api_doc.mustache");
            //Templates to decode response headers
            supportingFiles.add(new SupportingFile("model/ApiResponse.mustache", modelsFolder, "ApiResponse.java"));

            // TODO remove "file" from reserved word list as feign client doesn't support using `baseName`
            // as the parameter name yet
            reservedWords.remove("file");
        }

        if (!(libFeign || libRestTemplate || libRetrofit2 || libGoogleApiClient || libRestAssured || libWebClient
                || libMicroprofile || libRestClient)) {
            supportingFiles.add(new SupportingFile("apiException.mustache", invokerFolder, "ApiException.java"));
            supportingFiles.add(new SupportingFile("Configuration.mustache", invokerFolder, "Configuration.java"));
            supportingFiles.add(new SupportingFile("Pair.mustache", invokerFolder, "Pair.java"));
        }

        if (!(libFeign || libRestTemplate || libRetrofit2 || libGoogleApiClient || libRestAssured || libNative || libMicroprofile)) {
            supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));
        }

        if (libApache || libRestTemplate) {
            supportingFiles.add(new SupportingFile("BaseApi.mustache", invokerFolder, "BaseApi.java"));
        }

        if (libFeign) {
            if (getSerializationLibrary() == null) {
                LOGGER.info("No serializationLibrary configured, using '{}' as fallback", SERIALIZATION_LIBRARY_JACKSON);
                setSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
            }
            if (SERIALIZATION_LIBRARY_JACKSON.equals(getSerializationLibrary())) {
                supportingFiles.add(new SupportingFile("ApiResponseDecoder.mustache", invokerFolder, "ApiResponseDecoder.java"));
                supportingFiles.add(new SupportingFile("ParamExpander.mustache", invokerFolder, "ParamExpander.java"));
            }
            supportingFiles.add(new SupportingFile("EncodingUtils.mustache", invokerFolder, "EncodingUtils.java"));

            // Composed schemas can have the 'additionalProperties' keyword, as specified in JSON schema.
            // In principle, this should be enabled by default for all code generators. However due to limitations
            // in other code generators, support needs to be enabled on a case-by-case basis.
            // The flag below should be set for all Java libraries, but the templates need to be ported
            // one by one for each library.
            supportsAdditionalPropertiesWithComposedSchema = true;
        } else if (libOkHttpGson) {
            // the "okhttp-gson" library template requires "ApiCallback.mustache" for async call
            supportingFiles.add(new SupportingFile("ApiCallback.mustache", invokerFolder, "ApiCallback.java"));
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("ProgressRequestBody.mustache", invokerFolder, "ProgressRequestBody.java"));
            supportingFiles.add(new SupportingFile("ProgressResponseBody.mustache", invokerFolder, "ProgressResponseBody.java"));
            supportingFiles.add(new SupportingFile("GzipRequestInterceptor.mustache", invokerFolder, "GzipRequestInterceptor.java"));
            supportingFiles.add(new SupportingFile("AbstractOpenApiSchema.mustache", modelsFolder, "AbstractOpenApiSchema.java"));

            // NOTE: below moved to postProcessOperationsWithModels
            //supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
            //supportingFiles.add(new SupportingFile("auth/RetryingOAuth.mustache", authFolder, "RetryingOAuth.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_GSON);

            // Composed schemas can have the 'additionalProperties' keyword, as specified in JSON schema.
            // In principle, this should be enabled by default for all code generators. However due to limitations
            // in other code generators, support needs to be enabled on a case-by-case basis.
            // The flag below should be set for all Java libraries, but the templates need to be ported
            // one by one for each library.
            supportsAdditionalPropertiesWithComposedSchema = true;
        } else if (libRetrofit2) {
            supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
            supportingFiles.add(new SupportingFile("CollectionFormats.mustache", invokerFolder, "CollectionFormats.java"));
            if (SERIALIZATION_LIBRARY_JACKSON.equals(getSerializationLibrary())) {
                supportingFiles.add(new SupportingFile("JSON_jackson.mustache", invokerFolder, "JSON.java"));
            } else if (!usePlayWS) {
                supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            }
        } else if (libJersey2) {
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
        } else if (libJersey3) {
            additionalProperties.put("jersey3", true);
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
            applyJakartaPackage();
        } else if (libNative) {
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("AbstractOpenApiSchema.mustache", modelsFolder, "AbstractOpenApiSchema.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        } else if (libRestEasy) {
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        } else if (libRestTemplate) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
            supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));

            // Composed schemas can have the 'additionalProperties' keyword, as specified in JSON schema.
            // In principle, this should be enabled by default for all code generators. However due to limitations
            // in other code generators, support needs to be enabled on a case-by-case basis.
            // The flag below should be set for all Java libraries, but the templates need to be ported
            // one by one for each library.
            supportsAdditionalPropertiesWithComposedSchema = true;
        } else if (libWebClient) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);

            // Composed schemas can have the 'additionalProperties' keyword, as specified in JSON schema.
            // In principle, this should be enabled by default for all code generators. However due to limitations
            // in other code generators, support needs to be enabled on a case-by-case basis.
            // The flag below should be set for all Java libraries, but the templates need to be ported
            // one by one for each library.
            supportsAdditionalPropertiesWithComposedSchema = true;
        } else if (libRestClient) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
            applyJakartaPackage();

            // Composed schemas can have the 'additionalProperties' keyword, as specified in JSON schema.
            // In principle, this should be enabled by default for all code generators. However due to limitations
            // in other code generators, support needs to be enabled on a case-by-case basis.
            // The flag below should be set for all Java libraries, but the templates need to be ported
            // one by one for each library.
            supportsAdditionalPropertiesWithComposedSchema = true;
        } else if (libVertx) {
            typeMapping.put("file", "AsyncFile");
            importMapping.put("AsyncFile", "io.vertx.core.file.AsyncFile");
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
            apiTemplateFiles.put("apiImpl.mustache", "Impl.java");
            apiTemplateFiles.put("rxApiImpl.mustache", ".java");
            supportingFiles.remove(new SupportingFile("manifest.mustache", projectFolder, "AndroidManifest.xml"));
        } else if (libGoogleApiClient) {
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        } else if (libRestAssured) {
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
        } else if (libMicroprofile) {
            supportingFiles.clear(); // Don't need extra files provided by Java Codegen
            String apiExceptionFolder = (sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
            String pomTemplate = mpRestClientVersions.get(microprofileRestClientVersion).pomTemplate;
            supportingFiles.add(new SupportingFile(pomTemplate, "", "pom.xml"));
            supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
            supportingFiles.add(new SupportingFile("api_exception.mustache", apiExceptionFolder, "ApiException.java"));
            supportingFiles.add(new SupportingFile("api_exception_mapper.mustache", apiExceptionFolder, "ApiExceptionMapper.java"));
            if (getSerializationLibrary() == null) {
                LOGGER.info("No serializationLibrary configured, using '{}' as fallback", SERIALIZATION_LIBRARY_JSONB);
                setSerializationLibrary(SERIALIZATION_LIBRARY_JSONB);
            } else if (getSerializationLibrary().equals(SERIALIZATION_LIBRARY_GSON)) {
                forceSerializationLibrary(SERIALIZATION_LIBRARY_JSONB);
            }

            // currently not supported for Microprofile (neither for Jackson nor JSON-B)
            openApiNullable = false;
            additionalProperties.put(OPENAPI_NULLABLE, false);

            if (microprofileFramework.equals(MICROPROFILE_KUMULUZEE)) {
                supportingFiles.add(new SupportingFile("kumuluzee.pom.mustache", "", "pom.xml"));
                supportingFiles.add(new SupportingFile("kumuluzee.config.yaml.mustache", "src/main/resources", "config.yaml"));
                supportingFiles.add(new SupportingFile("kumuluzee.beans.xml.mustache", "src/main/resources/META-INF", "beans.xml"));
            }

            if ("3.0".equals(microprofileRestClientVersion)) {
                additionalProperties.put("microprofile3", true);
                if (getSerializationLibrary().equals(SERIALIZATION_LIBRARY_JSONB)) {
                    additionalProperties.put("jsonbPolymorphism", true);
                }
            }
        } else if (libApache) {
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
                supportingFiles.add(new SupportingFile("RFC3339InstantDeserializer.mustache", invokerFolder, "RFC3339InstantDeserializer.java"));
                supportingFiles.add(new SupportingFile("RFC3339JavaTimeModule.mustache", invokerFolder, "RFC3339JavaTimeModule.java"));
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
        
        if (isLibrary(FEIGN)) {
            additionalProperties.put("feign-okhttp", "true");
        } else if (isLibrary(FEIGN_HC5)) {
            additionalProperties.put("feign-hc5", "true");
            setTemplateDir(FEIGN);
            setLibrary(FEIGN);
        }

        // authentication related files
        // has OAuth defined
        if (ProcessUtils.hasOAuthMethods(openAPI)) {
            // for okhttp-gson (default), check to see if OAuth is defined and included OAuth-related files accordingly
            if (libOkHttpGson) {
                supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
                supportingFiles.add(new SupportingFile("auth/RetryingOAuth.mustache", authFolder, "RetryingOAuth.java"));
            }

            // google-api-client doesn't use the OpenAPI auth, because it uses Google Credential directly (HttpRequestInitializer)
            if (!(libGoogleApiClient || libRestAssured || usePlayWS || libNative || libMicroprofile)) {
                supportingFiles.add(new SupportingFile("auth/OAuth.mustache", authFolder, "OAuth.java"));
                supportingFiles.add(new SupportingFile("auth/OAuthFlow.mustache", authFolder, "OAuthFlow.java"));
            }

            // Add OauthPasswordGrant.java and OauthClientCredentialsGrant.java for feign library
            if (libFeign) {
                supportingFiles.add(new SupportingFile("auth/DefaultApi20Impl.mustache", authFolder, "DefaultApi20Impl.java"));
                supportingFiles.add(new SupportingFile("auth/OauthPasswordGrant.mustache", authFolder, "OauthPasswordGrant.java"));
                supportingFiles.add(new SupportingFile("auth/OauthClientCredentialsGrant.mustache", authFolder, "OauthClientCredentialsGrant.java"));
                supportingFiles.add(new SupportingFile("auth/ApiErrorDecoder.mustache", authFolder, "ApiErrorDecoder.java"));
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);

        if (this.getSingleRequestParameter() && (isLibrary(JERSEY2) || isLibrary(JERSEY3) || isLibrary(OKHTTP_GSON))) {
            // loop through operations to set x-group-parameters extension to true if useSingleRequestParameter option is enabled
            OperationMap operations = objs.getOperations();
            if (operations != null) {
                List<CodegenOperation> ops = operations.getOperation();
                for (CodegenOperation operation : ops) {
                    if (!operation.vendorExtensions.containsKey("x-group-parameters")) {
                        operation.vendorExtensions.put("x-group-parameters", true);
                    }
                }
            }
        }

        if (isLibrary(RETROFIT_2)) {
            OperationMap operations = objs.getOperations();
            if (operations != null) {
                List<CodegenOperation> ops = operations.getOperation();
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
        if (isLibrary(FEIGN) || isLibrary(FEIGN_HC5)) {
            OperationMap operations = objs.getOperations();
            List<CodegenOperation> operationList = operations.getOperation();
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
                        items[i] = "{" + camelize(items[i].substring(1, items[i].length() - 1), LOWERCASE_FIRST_LETTER) + "}";
                    }
                }
                op.path = StringUtils.join(items, "/");
                // Replace the custom method on the path if one was found earlier
                if (!method.isEmpty()) {
                    op.path += ":" + method;
                }
            }
        }

        if (isLibrary(NATIVE) || isLibrary(APACHE)) {
            OperationMap operations = objs.getOperations();
            List<CodegenOperation> operationList = operations.getOperation();
            for (CodegenOperation op : operationList) {
                // add extension to indicate content type is `text/plain` and the response type is `String`
                if ("String".equals(op.returnType) && op.producesTextPlain()) {
                    op.vendorExtensions.put("x-java-text-plain-string", true);
                }
            }
        }

        if (isLibrary(MICROPROFILE)) {
            objs = AbstractJavaJAXRSServerCodegen.jaxrsPostProcessOperations(objs);
            if (configKeyFromClassName) {
                Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
                String configKeyFromClassName = operations.get("classname")
                        .toString()
                        .replaceFirst("Api", "")
                        .toLowerCase(Locale.ROOT).concat("-api");
                operations.put("configKey", configKeyFromClassName);
            }
        }

        if (isLibrary(WEBCLIENT)) {
            OperationMap operations = objs.getOperations();
            if (operations != null) {
                List<CodegenOperation> ops = operations.getOperation();
                for (CodegenOperation operation : ops) {
                    if (!operation.vendorExtensions.containsKey(VendorExtension.X_WEBCLIENT_BLOCKING.getName()) && webclientBlockingOperations) {
                        operation.vendorExtensions.put(VendorExtension.X_WEBCLIENT_BLOCKING.getName(), true);
                    }

                    if (operation.isArray && !"string".equalsIgnoreCase(operation.returnBaseType)) {
                        operation.vendorExtensions.put(VendorExtension.X_WEBCLIENT_RETURN_EXCEPT_LIST_OF_STRING.getName(), true);
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        if (isLibrary(VERTX)) {
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
        if (!model.isEnum) {
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

        if (isLibrary(MICROPROFILE)) {
            model.imports.remove("ApiModelProperty");
            model.imports.remove("ApiModel");
        }

        if (!model.isEnum) {
            // needed by all pojos, but not enums
            if (AnnotationLibrary.SWAGGER2.equals(getAnnotationLibrary())) {
                model.imports.add("Schema");
            }
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
        if (isLibrary(MICROPROFILE)) {
            if (codegenModel.imports.contains("ApiModel")) {
                // Remove io.swagger.annotations.ApiModel import
                codegenModel.imports.remove("ApiModel");
            }
        }

        // TODO: inverse logic. Do not add the imports unconditionally in the first place.
        if (!AnnotationLibrary.SWAGGER1.equals(getAnnotationLibrary())) {
            // Remove io.swagger.annotations.* imports
            codegenModel.imports.remove("ApiModel");
            codegenModel.imports.remove("ApiModelProperty");
        }

        if (codegenModel.description != null) {
            if (AnnotationLibrary.SWAGGER2.equals(getAnnotationLibrary())) {
                codegenModel.imports.add("Schema");
            }
        }

        return codegenModel;
    }

    @Override
    public ModelsMap postProcessModelsEnum(ModelsMap objs) {
        objs = super.postProcessModelsEnum(objs);
        //Needed import for Gson based libraries
        if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_GSON)) {
            List<Map<String, String>> imports = objs.getImports();
            for (ModelMap mo : objs.getModels()) {
                CodegenModel cm = mo.getModel();
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
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);
        List<ModelMap> models = objs.getModels();

        if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_JACKSON)) {
            List<Map<String, String>> imports = objs.getImports();
            for (ModelMap mo : models) {
                CodegenModel cm = mo.getModel();
                boolean addNullableImports = false;

                for (CodegenProperty var : cm.vars) {
                    addNullableImports = isAddNullableImports(cm, addNullableImports, var);
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
                        Map<String, String> importsSet = new HashMap<>();
                        importsSet.put("import", "java.util.Set");
                        imports.add(importsSet);
                        Map<String, String> importsHashSet = new HashMap<>();
                        importsHashSet.put("import", "java.util.HashSet");
                        imports.add(importsHashSet);
                    }

                }

                if (addNullableImports) {
                    Map<String, String> imports2Classnames = new HashMap<>();
                    imports2Classnames.put("JsonNullable", "org.openapitools.jackson.nullable.JsonNullable");
                    imports2Classnames.put("NoSuchElementException", "java.util.NoSuchElementException");
                    imports2Classnames.put("JsonIgnore", "com.fasterxml.jackson.annotation.JsonIgnore");
                    addImports(imports, cm, imports2Classnames);
                }
            }
        }

        // add implements for serializable/parcelable to all models
        for (ModelMap mo : models) {
            CodegenModel cm = mo.getModel();

            cm.getVendorExtensions().putIfAbsent("x-implements", new ArrayList<String>());
            if (isLibrary(JERSEY2) || isLibrary(JERSEY3) || isLibrary(NATIVE) || isLibrary(OKHTTP_GSON)) {
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
            if (this.parcelableModel && !cm.isEnum) {
                ((ArrayList<String>) cm.getVendorExtensions().get("x-implements")).add("Parcelable");
            }
        }

        return objs;
    }

    @Override
    protected boolean isConstructorWithAllArgsAllowed(CodegenModel codegenModel) {
        // implementation detail: allVars is not reliable if openapiNormalizer.REFACTOR_ALLOF_WITH_PROPERTIES_ONLY is disabled
        if (codegenModel.readOnlyVars.size() != codegenModel.vars.size() + codegenModel.parentVars.size()) {
            return super.isConstructorWithAllArgsAllowed(codegenModel);
        }
        return false;
    }

    public boolean getUseOneOfDiscriminatorLookup() {
        return this.useOneOfDiscriminatorLookup;
    }

    public boolean getSingleRequestParameter() {
        return "true".equals(getUseSingleRequestParameter()) || "static".equals(getUseSingleRequestParameter());
    }

    public boolean getStaticRequest() {
        return "static".equals(this.getUseSingleRequestParameter());
    }

    private String getUseSingleRequestParameter() {
        return useSingleRequestParameter;
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

    public void setCaseInsensitiveResponseHeaders(final Boolean caseInsensitiveResponseHeaders) {
        this.caseInsensitiveResponseHeaders = caseInsensitiveResponseHeaders;
    }

    public void setSerializationLibrary(String serializationLibrary) {
        if (SERIALIZATION_LIBRARY_JACKSON.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_JACKSON;
            this.jackson = true;
        } else if (SERIALIZATION_LIBRARY_GSON.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_GSON;
            this.jackson = false;
        } else if (SERIALIZATION_LIBRARY_JSONB.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_JSONB;
            this.jackson = false;
        } else {
            throw new IllegalArgumentException("Unexpected serializationLibrary value: " + serializationLibrary);
        }
    }

    public void forceSerializationLibrary(String serializationLibrary) {
        if (this.serializationLibrary != null && !this.serializationLibrary.equalsIgnoreCase(serializationLibrary)) {
            LOGGER.warn("The configured serializationLibrary '{}', is not supported by the library: '{}', switching back to: {}",
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
