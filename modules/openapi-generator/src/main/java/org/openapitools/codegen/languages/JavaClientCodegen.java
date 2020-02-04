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

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.GzipFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.templating.mustache.CaseFormatLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;

import java.io.File;
import java.util.*;
import java.util.regex.Pattern;

import static com.google.common.base.CaseFormat.LOWER_CAMEL;
import static com.google.common.base.CaseFormat.UPPER_UNDERSCORE;
import static java.util.Collections.sort;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class JavaClientCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures, GzipFeatures {

    static final String MEDIA_TYPE = "mediaType";

    private static final Logger LOGGER = LoggerFactory.getLogger(JavaClientCodegen.class);

    public static final String USE_RX_JAVA = "useRxJava";
    public static final String USE_RX_JAVA2 = "useRxJava2";
    public static final String DO_NOT_USE_RX = "doNotUseRx";
    public static final String USE_PLAY_WS = "usePlayWS";
    public static final String PLAY_VERSION = "playVersion";
    public static final String FEIGN_VERSION = "feignVersion";
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
    public static final String RETROFIT_1 = "retrofit";
    public static final String RETROFIT_2 = "retrofit2";
    public static final String VERTX = "vertx";
    public static final String MICROPROFILE = "microprofile";

    public static final String SERIALIZATION_LIBRARY_GSON = "gson";
    public static final String SERIALIZATION_LIBRARY_JACKSON = "jackson";

    protected String gradleWrapperPackage = "gradle.wrapper";
    protected boolean useRxJava = false;
    protected boolean useRxJava2 = false;
    // backwards compatibility for openapi configs that specify neither rx1 nor rx2
    // (mustache does not allow for boolean operators so we need this extra field)
    protected boolean doNotUseRx = true;
    protected boolean usePlayWS = false;
    protected String playVersion = PLAY_25;
    protected String feignVersion = FEIGN_9;
    protected boolean parcelableModel = false;
    protected boolean useBeanValidation = false;
    protected boolean performBeanValidation = false;
    protected boolean useGzipFeature = false;
    protected boolean useRuntimeException = false;
    protected boolean useReflectionEqualsHashCode = false;
    protected boolean caseInsensitiveResponseHeaders = false;
    protected String authFolder;
    protected String serializationLibrary = null;

    protected boolean useOneOfInterfaces = false;
    protected List<CodegenModel> addOneOfInterfaces = new ArrayList<CodegenModel>();

    public JavaClientCodegen() {
        super();

        featureSet = getFeatureSet().modify()
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .build();

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

        cliOptions.add(CliOption.newBoolean(USE_RX_JAVA, "Whether to use the RxJava adapter with the retrofit2 library."));
        cliOptions.add(CliOption.newBoolean(USE_RX_JAVA2, "Whether to use the RxJava2 adapter with the retrofit2 library."));
        cliOptions.add(CliOption.newBoolean(PARCELABLE_MODEL, "Whether to generate models for Android that implement Parcelable with the okhttp-gson library."));
        cliOptions.add(CliOption.newBoolean(USE_PLAY_WS, "Use Play! Async HTTP client (Play WS API)"));
        cliOptions.add(CliOption.newString(PLAY_VERSION, "Version of Play! Framework (possible values \"play24\", \"play25\" (default), \"play26\")"));
        cliOptions.add(CliOption.newBoolean(SUPPORT_JAVA6, "Whether to support Java6 with the Jersey1 library."));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));
        cliOptions.add(CliOption.newBoolean(PERFORM_BEANVALIDATION, "Perform BeanValidation"));
        cliOptions.add(CliOption.newBoolean(USE_GZIP_FEATURE, "Send gzip-encoded requests"));
        cliOptions.add(CliOption.newBoolean(USE_RUNTIME_EXCEPTION, "Use RuntimeException instead of Exception"));
        cliOptions.add(CliOption.newBoolean(FEIGN_VERSION, "Version of OpenFeign: '10.x', '9.x' (default)"));
        cliOptions.add(CliOption.newBoolean(USE_REFLECTION_EQUALS_HASHCODE, "Use org.apache.commons.lang3.builder for equals and hashCode in the models. WARNING: This will fail under a security manager, unless the appropriate permissions are set up correctly and also there's potential performance impact."));
        cliOptions.add(CliOption.newBoolean(CASE_INSENSITIVE_RESPONSE_HEADERS, "Make API response's headers case-insensitive. Available on " + OKHTTP_GSON + ", " + JERSEY2 + " libraries"));

        supportedLibraries.put(JERSEY1, "HTTP client: Jersey client 1.19.x. JSON processing: Jackson 2.9.x. Enable Java6 support using '-DsupportJava6=true'. Enable gzip request encoding using '-DuseGzipFeature=true'. IMPORTANT NOTE: jersey 1.x is no longer actively maintained so please upgrade to 'jersey2' or other HTTP libaries instead.");
        supportedLibraries.put(JERSEY2, "HTTP client: Jersey client 2.25.1. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(FEIGN, "HTTP client: OpenFeign 9.x or 10.x. JSON processing: Jackson 2.9.x. To enable OpenFeign 10.x, set the 'feignVersion' option to '10.x'");
        supportedLibraries.put(OKHTTP_GSON, "[DEFAULT] HTTP client: OkHttp 3.x. JSON processing: Gson 2.8.x. Enable Parcelable models on Android using '-DparcelableModel=true'. Enable gzip request encoding using '-DuseGzipFeature=true'.");
        supportedLibraries.put(RETROFIT_1, "HTTP client: OkHttp 2.x. JSON processing: Gson 2.x (Retrofit 1.9.0). IMPORTANT NOTE: retrofit1.x is no longer actively maintained so please upgrade to 'retrofit2' instead.");
        supportedLibraries.put(RETROFIT_2, "HTTP client: OkHttp 3.x. JSON processing: Gson 2.x (Retrofit 2.3.0). Enable the RxJava adapter using '-DuseRxJava[2]=true'. (RxJava 1.x or 2.x)");
        supportedLibraries.put(RESTTEMPLATE, "HTTP client: Spring RestTemplate 4.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(WEBCLIENT, "HTTP client: Spring WebClient 5.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(RESTEASY, "HTTP client: Resteasy client 3.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(VERTX, "HTTP client: VertX client 3.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(GOOGLE_API_CLIENT, "HTTP client: Google API client 1.x. JSON processing: Jackson 2.9.x");
        supportedLibraries.put(REST_ASSURED, "HTTP client: rest-assured : 4.x. JSON processing: Gson 2.x or Jackson 2.9.x. Only for Java8");
        supportedLibraries.put(NATIVE, "HTTP client: Java native HttpClient. JSON processing: Jackson 2.9.x. Only for Java11+");
        supportedLibraries.put(MICROPROFILE, "HTTP client: Microprofile client X.x. JSON processing: Jackson 2.9.x");

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
        return "Generates a Java client library (HTTP lib: Jersey (1.x, 2.x), Retrofit (1.x, 2.x), OpenFeign (9.x, 10.x) and more.";
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
        }
        else if (MICROPROFILE.equals(getLibrary())) {
            dateLibrary = "legacy";
        }

        super.processOpts();

        // RxJava
        if (additionalProperties.containsKey(USE_RX_JAVA) && additionalProperties.containsKey(USE_RX_JAVA2)) {
            LOGGER.warn("You specified both RxJava versions 1 and 2 but they are mutually exclusive. Defaulting to v2.");
        } else if (additionalProperties.containsKey(USE_RX_JAVA)) {
            this.setUseRxJava(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA).toString()));
        }
        if (additionalProperties.containsKey(USE_RX_JAVA2)) {
            this.setUseRxJava2(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA2).toString()));
        }
        if (!useRxJava && !useRxJava2) {
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

        // OpenFeign
        if (additionalProperties.containsKey(FEIGN_VERSION)) {
            this.setFeignVersion(additionalProperties.get(FEIGN_VERSION).toString());

            if ("10.x".equals(feignVersion)) {
                additionalProperties.put("useFeign10", true);
            } else if ("9.x".equals(feignVersion)) {
                // do nothing as 9.x is the default
            } else {
                throw new RuntimeException("Ivalid feignOoption '{}'. Must be '10.x' or '9.x'.");
            }
        }
        additionalProperties.put(FEIGN_VERSION, feignVersion);

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
        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
        writeOptional(outputFolder, new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        writeOptional(outputFolder, new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        writeOptional(outputFolder, new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        writeOptional(outputFolder, new SupportingFile("gradle.properties.mustache", "", "gradle.properties"));
        writeOptional(outputFolder, new SupportingFile("manifest.mustache", projectFolder, "AndroidManifest.xml"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache", invokerFolder, "ApiClient.java"));
        if (!(RESTTEMPLATE.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || NATIVE.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) {
            supportingFiles.add(new SupportingFile("StringUtil.mustache", invokerFolder, "StringUtil.java"));
        }

        // google-api-client doesn't use the OpenAPI auth, because it uses Google Credential directly (HttpRequestInitializer)
        if (!(GOOGLE_API_CLIENT.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || NATIVE.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) {
            supportingFiles.add(new SupportingFile("auth/HttpBasicAuth.mustache", authFolder, "HttpBasicAuth.java"));
            supportingFiles.add(new SupportingFile("auth/HttpBearerAuth.mustache", authFolder, "HttpBearerAuth.java"));
            supportingFiles.add(new SupportingFile("auth/ApiKeyAuth.mustache", authFolder, "ApiKeyAuth.java"));
            // NOTE: below moved to postProcessOperationsWithModels
            //supportingFiles.add(new SupportingFile("auth/OAuth.mustache", authFolder, "OAuth.java"));
            //supportingFiles.add(new SupportingFile("auth/OAuthFlow.mustache", authFolder, "OAuthFlow.java"));
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

        //TODO: add doc to retrofit1 and feign
        if (FEIGN.equals(getLibrary()) || RETROFIT_1.equals(getLibrary())) {
            modelDocTemplateFiles.remove("model_doc.mustache");
            apiDocTemplateFiles.remove("api_doc.mustache");
        }

        if (!(FEIGN.equals(getLibrary()) || RESTTEMPLATE.equals(getLibrary()) || usesAnyRetrofitLibrary() || GOOGLE_API_CLIENT.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || WEBCLIENT.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) {
            supportingFiles.add(new SupportingFile("apiException.mustache", invokerFolder, "ApiException.java"));
            supportingFiles.add(new SupportingFile("Configuration.mustache", invokerFolder, "Configuration.java"));
            supportingFiles.add(new SupportingFile("Pair.mustache", invokerFolder, "Pair.java"));
        }

        if (!(FEIGN.equals(getLibrary()) || RESTTEMPLATE.equals(getLibrary()) || usesAnyRetrofitLibrary() || GOOGLE_API_CLIENT.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || NATIVE.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) {
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

            // NOTE: below moved to postProcessOpoerationsWithModels
            //supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
            //supportingFiles.add(new SupportingFile("auth/RetryingOAuth.mustache", authFolder, "RetryingOAuth.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_GSON);
        } else if (usesAnyRetrofitLibrary()) {
            supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
            supportingFiles.add(new SupportingFile("CollectionFormats.mustache", invokerFolder, "CollectionFormats.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_GSON);
            if ("retrofit2".equals(getLibrary()) && !usePlayWS) {
                supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            }
        } else if (JERSEY2.equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            forceSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
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
            if(getSerializationLibrary() == null) {
                LOGGER.info("No serializationLibrary configured, using '"+SERIALIZATION_LIBRARY_GSON+"' as fallback");
                setSerializationLibrary(SERIALIZATION_LIBRARY_GSON);
            }
            if(SERIALIZATION_LIBRARY_JACKSON.equals(getSerializationLibrary())) {
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
                additionalProperties.put(PLAY_24, true);
                apiTemplateFiles.put("play24/api.mustache", ".java");

                supportingFiles.add(new SupportingFile("play24/ApiClient.mustache", invokerFolder, "ApiClient.java"));
                supportingFiles.add(new SupportingFile("play24/Play24CallFactory.mustache", invokerFolder, "Play24CallFactory.java"));
                supportingFiles.add(new SupportingFile("play24/Play24CallAdapterFactory.mustache", invokerFolder,
                        "Play24CallAdapterFactory.java"));
            }

            if (PLAY_25.equals(playVersion)) {
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

        if(getSerializationLibrary() == null) {
            LOGGER.info("No serializationLibrary configured, using '"+SERIALIZATION_LIBRARY_GSON+"' as fallback");
            setSerializationLibrary(SERIALIZATION_LIBRARY_GSON);
        }
        if(SERIALIZATION_LIBRARY_JACKSON.equals(getSerializationLibrary())) {
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

        if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_JACKSON)) {
            useOneOfInterfaces = true;
        }

    }

    private boolean usesAnyRetrofitLibrary() {
        return getLibrary() != null && getLibrary().contains(RETROFIT_1);
    }

    private boolean usesRetrofit2Library() {
        return getLibrary() != null && getLibrary().contains(RETROFIT_2);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        if (usesAnyRetrofitLibrary()) {
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
                    if (usesRetrofit2Library() && StringUtils.isNotEmpty(operation.path) && operation.path.startsWith("/")) {
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

        // for okhttp-gson (default), check to see if OAuth is defined and included OAuth-related files accordingly
        if ((OKHTTP_GSON.equals(getLibrary()) || StringUtils.isEmpty(getLibrary())) && ProcessUtils.hasOAuthMethods(objs)) {
            supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
            supportingFiles.add(new SupportingFile("auth/RetryingOAuth.mustache", authFolder, "RetryingOAuth.java"));
        }

        // google-api-client doesn't use the OpenAPI auth, because it uses Google Credential directly (HttpRequestInitializer)
        if ((!(GOOGLE_API_CLIENT.equals(getLibrary()) || REST_ASSURED.equals(getLibrary()) || usePlayWS || NATIVE.equals(getLibrary()) || MICROPROFILE.equals(getLibrary()))) && ProcessUtils.hasOAuthMethods(objs)) {
            supportingFiles.add(new SupportingFile("auth/OAuth.mustache", authFolder, "OAuth.java"));
            supportingFiles.add(new SupportingFile("auth/OAuthFlow.mustache", authFolder, "OAuthFlow.java"));
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
                    var.getVendorExtensions().put("isJacksonOptionalNullable", isOptionalNullable);
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
            cm.getVendorExtensions().putIfAbsent("implements", new ArrayList<String>());
            List<String> impl = (List<String>) cm.getVendorExtensions().get("implements");
            if (this.parcelableModel) {
                impl.add("Parcelable");
            }
            if (this.serializableModel) {
                impl.add("Serializable");
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
     * @return 'gson' or 'jackson'
     */
    public String getSerializationLibrary() {
        return serializationLibrary;
    }

    public void setSerializationLibrary(String serializationLibrary) {
        if(SERIALIZATION_LIBRARY_JACKSON.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_JACKSON;
        } else if(SERIALIZATION_LIBRARY_GSON.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_GSON;
        } else {
            throw new IllegalArgumentException("Unexpected serializationLibrary value: " + serializationLibrary);
        }
    }

    public void addOneOfNameExtension(Schema s, String name) {
        ComposedSchema cs = (ComposedSchema) s;
        if (cs.getOneOf() != null && cs.getOneOf().size() > 0) {
            cs.addExtension("x-oneOf-name", name);
        }
    }

    public void addOneOfInterfaceModel(ComposedSchema cs, String type) {
        CodegenModel cm = new CodegenModel();

        for (Schema o : cs.getOneOf()) {
            // TODO: inline objects
            cm.oneOf.add(toModelName(ModelUtils.getSimpleRef(o.get$ref())));
        }
        cm.name = type;
        cm.classname = type;
        cm.vendorExtensions.put("isOneOfInterface", true);
        cm.discriminator = createDiscriminator("", (Schema) cs);
        cm.interfaceModels = new ArrayList<CodegenModel>();

        addOneOfInterfaces.add(cm);
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        // we process the openapi schema here to find oneOf schemas here and create interface models for them
        super.preprocessOpenAPI(openAPI);
        Map<String, Schema> schemas = new HashMap<String, Schema>(openAPI.getComponents().getSchemas());
        if (schemas == null) {
            schemas = new HashMap<String, Schema>();
        }
        Map<String, PathItem> pathItems = openAPI.getPaths();

        // we need to add all request and response bodies to processed schemas
        if (pathItems != null) {
            for (Map.Entry<String, PathItem> e : pathItems.entrySet()) {
                for (Map.Entry<PathItem.HttpMethod, Operation> op : e.getValue().readOperationsMap().entrySet()) {
                    String opId = getOrGenerateOperationId(op.getValue(), e.getKey(), op.getKey().toString());
                    // process request body
                    RequestBody b = ModelUtils.getReferencedRequestBody(openAPI, op.getValue().getRequestBody());
                    Schema requestSchema = null;
                    if (b != null) {
                        requestSchema = ModelUtils.getSchemaFromRequestBody(b);
                    }
                    if (requestSchema != null) {
                        schemas.put(opId, requestSchema);
                    }
                    // process all response bodies
                    for (Map.Entry<String, ApiResponse> ar : op.getValue().getResponses().entrySet()) {
                        ApiResponse a = ModelUtils.getReferencedApiResponse(openAPI, ar.getValue());
                        Schema responseSchema = ModelUtils.getSchemaFromResponse(a);
                        if (responseSchema != null) {
                            schemas.put(opId + ar.getKey(), responseSchema);
                        }
                    }
                }
            }
        }

        for (Map.Entry<String, Schema> e : schemas.entrySet()) {
            String n = toModelName(e.getKey());
            Schema s = e.getValue();
            String nOneOf = toModelName(n + "OneOf");
            if (ModelUtils.isComposedSchema(s)) {
                addOneOfNameExtension(s, n);
            } else if (ModelUtils.isArraySchema(s)) {
                Schema items = ((ArraySchema) s).getItems();
                if (ModelUtils.isComposedSchema(items)) {
                    addOneOfNameExtension(items, nOneOf);
                    addOneOfInterfaceModel((ComposedSchema) items, nOneOf);
                }
            } else if (ModelUtils.isMapSchema(s)) {
                Schema addProps = ModelUtils.getAdditionalProperties(s);
                if (addProps != null && ModelUtils.isComposedSchema(addProps)) {
                    addOneOfNameExtension(addProps, nOneOf);
                    addOneOfInterfaceModel((ComposedSchema) addProps, nOneOf);
                }
            }
        }
    }

    private class OneOfImplementorAdditionalData {
        private String implementorName;
        private List<String> additionalInterfaces = new ArrayList<String>();
        private List<CodegenProperty> additionalProps = new ArrayList<CodegenProperty>();
        private List<Map<String, String>> additionalImports = new ArrayList<Map<String, String>>();

        public OneOfImplementorAdditionalData(String implementorName) {
            this.implementorName = implementorName;
        }

        public String getImplementorName() {
            return implementorName;
        }

        public void addFromInterfaceModel(CodegenModel cm, List<Map<String, String>> modelsImports) {
            // Add cm as implemented interface
            additionalInterfaces.add(cm.classname);

            // Add all vars defined on cm
            // a "oneOf" model (cm) by default inherits all properties from its "interfaceModels",
            // but we only want to add properties defined on cm itself
            List<CodegenProperty> toAdd = new ArrayList<CodegenProperty>(cm.vars);
            // note that we can't just toAdd.removeAll(m.vars) for every interfaceModel,
            // as they might have different value of `hasMore` and thus are not equal
            List<String> omitAdding = new ArrayList<String>();
            for (CodegenModel m : cm.interfaceModels) {
                for (CodegenProperty v : m.vars) {
                    omitAdding.add(v.baseName);
                }
            }
            for (CodegenProperty v : toAdd) {
                if (!omitAdding.contains(v.baseName)) {
                    additionalProps.add(v.clone());
                }
            }

            // Add all imports of cm
            for (Map<String, String> importMap : modelsImports) {
                // we're ok with shallow clone here, because imports are strings only
                additionalImports.add(new HashMap<String, String>(importMap));
            }
        }

        public void addToImplementor(CodegenModel implcm, List<Map<String, String>> implImports) {
            implcm.getVendorExtensions().putIfAbsent("implements", new ArrayList<String>());

            // Add implemented interfaces
            for (String intf : additionalInterfaces) {
                List<String> impl = (List<String>) implcm.getVendorExtensions().get("implements");
                impl.add(intf);
                // Add imports for interfaces
                implcm.imports.add(intf);
                Map<String, String> importsItem = new HashMap<String, String>();
                importsItem.put("import", toModelImport(intf));
                implImports.add(importsItem);
            }

            // Add oneOf-containing models properties - we need to properly set the hasMore values to make renderind correct
            if (implcm.vars.size() > 0 && additionalProps.size() > 0) {
                implcm.vars.get(implcm.vars.size() - 1).hasMore = true;
            }
            for (int i = 0; i < additionalProps.size(); i++) {
                CodegenProperty var = additionalProps.get(i);
                if (i == additionalProps.size() - 1) {
                    var.hasMore = false;
                } else {
                    var.hasMore = true;
                }
                implcm.vars.add(var);
            }

            // Add imports
            for (Map<String, String> oneImport : additionalImports) {
                // exclude imports from this package - these are imports that only the oneOf interface needs
                if (!implImports.contains(oneImport) && !oneImport.getOrDefault("import", "").startsWith(modelPackage())) {
                    implImports.add(oneImport);
                }
            }
        }
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        objs = super.postProcessAllModels(objs);

        if (this.useOneOfInterfaces) {
            // First, add newly created oneOf interfaces
            for (CodegenModel cm : addOneOfInterfaces) {
                Map<String, Object> modelValue = new HashMap<String, Object>() {{
                    putAll(additionalProperties());
                    put("model", cm);
                }};
                List<Object> modelsValue = Arrays.asList(modelValue);
                List<Map<String, String>> importsValue = new ArrayList<Map<String, String>>();
                for (String i : Arrays.asList("JsonSubTypes", "JsonTypeInfo")) {
                    Map<String, String> oneImport = new HashMap<String, String>() {{
                        put("import", importMapping.get(i));
                    }};
                    importsValue.add(oneImport);
                }
                Map<String, Object> objsValue = new HashMap<String, Object>() {{
                    put("models", modelsValue);
                    put("package", modelPackage());
                    put("imports", importsValue);
                    put("classname", cm.classname);
                    putAll(additionalProperties);
                }};
                objs.put(cm.name, objsValue);
            }

            // - Add all "oneOf" models as interfaces to be implemented by the models that
            //   are the choices in "oneOf"; also mark the models containing "oneOf" as interfaces
            // - Add all properties of "oneOf" to the implementing classes (NOTE that this
            //   would be problematic if the class was in multiple such "oneOf" models, in which
            //   case it would get all their properties, but it's probably better than not doing this)
            // - Add all imports of "oneOf" model to all the implementing classes (this might not
            //   be optimal, as it can contain more than necessary, but it's good enough)
            Map<String, OneOfImplementorAdditionalData> additionalDataMap = new HashMap<String, OneOfImplementorAdditionalData>();
            for (Map.Entry modelsEntry : objs.entrySet()) {
                Map<String, Object> modelsAttrs = (Map<String, Object>) modelsEntry.getValue();
                List<Object> models = (List<Object>) modelsAttrs.get("models");
                List<Map<String, String>> modelsImports = (List<Map<String, String>>) modelsAttrs.getOrDefault("imports", new ArrayList<Map<String, String>>());
                for (Object _mo : models) {
                    Map<String, Object> mo = (Map<String, Object>) _mo;
                    CodegenModel cm = (CodegenModel) mo.get("model");
                    if (cm.oneOf.size() > 0) {
                        cm.vendorExtensions.put("isOneOfInterface", true);
                        // if this is oneOf interface, make sure we include the necessary jackson imports for it
                        for (String s : Arrays.asList("JsonTypeInfo", "JsonSubTypes")) {
                            Map<String, String> i = new HashMap<String, String>() {{
                                put("import", importMapping.get(s));
                            }};
                            if (!modelsImports.contains(i)) {
                                modelsImports.add(i);
                            }
                        }
                        for (String one : cm.oneOf) {
                            if (!additionalDataMap.containsKey(one)) {
                                additionalDataMap.put(one, new OneOfImplementorAdditionalData(one));
                            }
                            additionalDataMap.get(one).addFromInterfaceModel(cm, modelsImports);
                        }
                    }
                }
            }

            for (Map.Entry modelsEntry : objs.entrySet()) {
                Map<String, Object> modelsAttrs = (Map<String, Object>) modelsEntry.getValue();
                List<Object> models = (List<Object>) modelsAttrs.get("models");
                List<Map<String, String>> imports = (List<Map<String, String>>) modelsAttrs.get("imports");
                for (Object _implmo : models) {
                    Map<String, Object> implmo = (Map<String, Object>) _implmo;
                    CodegenModel implcm = (CodegenModel) implmo.get("model");
                    if (additionalDataMap.containsKey(implcm.name)) {
                        additionalDataMap.get(implcm.name).addToImplementor(implcm, imports);
                    }
                }
            }
        }

        return objs;
    }

    public void forceSerializationLibrary(String serializationLibrary) {
        if((this.serializationLibrary != null) && !this.serializationLibrary.equalsIgnoreCase(serializationLibrary)) {
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
    public String toApiVarName(String name) {
        String apiVarName = super.toApiVarName(name);
        if (reservedWords.contains(apiVarName)) {
            apiVarName = escapeReservedWord(apiVarName);
        }
        return apiVarName;
    }
}
