package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.openapitools.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class KotlinMultiplatformClientCodegen extends AbstractKotlinCodegen {
    public static final String PROJECT_NAME = "projectName";

    protected static final String VENDOR_EXTENSION_BASE_NAME_LITERAL = "x-base-name-literal";

    private static final Logger LOGGER = LoggerFactory.getLogger(KotlinMultiplatformClientCodegen.class);

    private static final String[] optionExcludes = new String[]{
            CodegenConstants.PARCELIZE_MODELS,
            CodegenConstants.SERIALIZABLE_MODEL,
            CodegenConstants.SERIALIZATION_LIBRARY
    };

    public static final class Options {
        // Versions
        public static final String KOTLIN_VERSION = "kotlinVersion";
        public static final String KTOR_VERSION = "ktorVersion";
        public static final String GRADLE_VERSION = "gradleVersion";
        public static final String ANDROID_GRADLE_VERSION = "androidGradleVersion";
        public static final String KOTLINX_DATETIME_VERSION = "kotlinxDatetimeVersion";

        // Platforms specific options
        public static final String JVM_ASYNC = "jvmAsync";
        public static final String ANDROID_ASYNC = "androidAsync";
        public static final String JS_ASYNC = "jsAsync";
        public static final String JS_BROWSER = "jsBrowser";
        public static final String JS_NODE = "jsNode";

        // Other options
        public static final String DATE_LIBRARY = "dateLibrary";
        public static final String SUBPROJECT = "subproject";

        public static final class Defaults {
            // Versions
            public static final String KOTLIN_VERSION = "1.4.10";
            public static final String KTOR_VERSION = "1.4.0";
            public static final String GRADLE_VERSION = "6.6.1";
            public static final String ANDROID_GRADLE_VERSION = "4.0.1";
            public static final String KOTLINX_DATETIME_VERSION = "0.1.0";

            // Platforms specific options
            // Default choices: https://www.jetbrains.com/lp/devecosystem-2020/kotlin/
            public static final boolean JVM_ENABLED = true;
            // Disabled by default, because it's harder to setup and the jvm client is normally sufficient for android
            public static final boolean ANDROID_ENABLED = false;
            public static final boolean JS_ENABLED = false;
            public static final boolean IOS_ENABLED = false;
            public static final boolean NATIVE_ENABLED = false;

            public static final JavaAsync JVM_ASYNC = JavaAsync.JDK8;
            public static final JavaAsync ANDROID_ASYNC = JavaAsync.NONE;
            public static final JsAsync JS_ASYNC = JsAsync.PROMISE;
            public static final boolean JS_BROWSER = true;
            public static final boolean JS_NODE = false;

            // Other options
            public static final DateLibrary DATE_LIBRARY = DateLibrary.STRING;
            public static final boolean SUBPROJECT = false;
        }

        // Enums
        public enum Platform {
            JVM("jvm"),
            ANDROID("android"),
            JS("js"),
            IOS("ios"),
            NATIVE("native");

            public final String value;

            Platform(String value) {
                this.value = value;
            }

            public static Platform fromName(String name) {
                return Arrays.stream(values())
                        .filter(v -> v.value.equals(name))
                        .findAny()
                        .orElseThrow(() -> new InvalidEnumNameException(name, "Platform", Arrays.stream(values()).map(Platform::value)));
            }

            public String value() {
                return value;
            }
        }

        public enum DateLibrary {
            STRING("string"),
            KOTLINX("kotlinx");

            public final String value;

            DateLibrary(String value) {
                this.value = value;
            }

            public static DateLibrary fromName(String name) {
                return Arrays.stream(values())
                        .filter(v -> v.value.equals(name))
                        .findAny()
                        .orElseThrow(() -> new InvalidEnumNameException(name, "DateLibrary", Arrays.stream(values()).map(DateLibrary::value)));
            }

            public String value() {
                return value;
            }
        }

        public enum JavaAsync {
            NONE("none"),
            // https://github.com/Kotlin/kotlinx.coroutines/blob/master/integration/kotlinx-coroutines-jdk8/README.md
            JDK8("jdk8");
            /*
            More possibilities:
            https://github.com/Kotlin/kotlinx.coroutines/blob/master/integration/README.md
             */

            public final String value;

            JavaAsync(String value) {
                this.value = value;
            }

            public static JavaAsync fromName(String name) {
                return Arrays.stream(values())
                        .filter(v -> v.value.equals(name))
                        .findAny()
                        .orElseThrow(() -> new InvalidEnumNameException(name, "JavaAsync", Arrays.stream(values()).map(JavaAsync::value)));
            }

            public String value() {
                return value;
            }
        }

        public enum JsAsync {
            NONE("none"),
            PROMISE("promise");

            public final String value;

            JsAsync(String value) {
                this.value = value;
            }

            public static JsAsync fromName(String name) {
                return Arrays.stream(values())
                        .filter(v -> v.value.equals(name))
                        .findAny()
                        .orElseThrow(() -> new InvalidEnumNameException(name, "JsAsync", Arrays.stream(values()).map(JsAsync::value)));
            }

            public String value() {
                return value;
            }
        }
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "kotlin-multiplatform";
    }

    public String getHelp() {
        return "Generates a kotlin-multiplatform client.";
    }

    public static CliOption newPlatformOption(Options.Platform platform, boolean defaultState) {
        return CliOption.newBoolean(
                platform.value + "Enabled", "Enables generation of core for " + platform.value + " in the multiplatform project",
                defaultState
        );
    }

    public KotlinMultiplatformClientCodegen() {
        super();

        /*
         * OAuth flows supported _only_ by client explicitly setting bearer token. The "flows" are not supported.
         */
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .excludeWireFormatFeatures(WireFormatFeature.XML, WireFormatFeature.PROTOBUF)
                .excludeSecurityFeatures(
                        SecurityFeature.OpenIDConnect,
                        SecurityFeature.OAuth2_Password,
                        SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_ClientCredentials,
                        SecurityFeature.OAuth2_Implicit
                )
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
                .includeClientModificationFeatures(ClientModificationFeature.BasePath)
        );

        artifactId = "kotlin-multiplatform-client";
        packageName = "org.openapitools.client";
        // https://kotlinlang.org/docs/reference/coding-conventions.html#property-names
        enumPropertyNaming = ENUM_PROPERTY_NAMING_TYPE.PascalCase;

        // cliOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.artifactId);
        updateOption(CodegenConstants.PACKAGE_NAME, this.packageName);
        updateOption(CodegenConstants.ENUM_PROPERTY_NAMING, this.enumPropertyNaming.name());

        sourceFolder = "src/common/main";
        outputFolder = "generated-code" + File.separator + "kotlin-multiplatform-client";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "kotlin-multiplatform-client";
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        cliOptions.removeIf(option -> Arrays.stream(optionExcludes).anyMatch(e -> e.equals(option.getOpt())));

        cliOptions.add(CliOption.newString(Options.KOTLIN_VERSION, "Sets the kotlin version used").defaultValue(Options.Defaults.KOTLIN_VERSION));
        cliOptions.add(CliOption.newString(Options.KTOR_VERSION, "Sets the ktor version used").defaultValue(Options.Defaults.KTOR_VERSION));
        cliOptions.add(CliOption.newString(Options.GRADLE_VERSION, "Sets the gradle version used").defaultValue(Options.Defaults.GRADLE_VERSION));
        cliOptions.add(CliOption.newString(Options.ANDROID_GRADLE_VERSION, "Sets the android gradle plugin version used").defaultValue(Options.Defaults.ANDROID_GRADLE_VERSION));

        CliOption dateLibrary = new CliOption(Options.DATE_LIBRARY, "Option. Date library to use");
        Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(Options.DateLibrary.KOTLINX.value, "Kotlinx multi-platform datetime library (experimental)");
        dateOptions.put(Options.DateLibrary.STRING.value, "String returns dates and times as string");
        dateLibrary.setEnum(dateOptions);
        dateLibrary.setDefault(Options.Defaults.DATE_LIBRARY.value);
        cliOptions.add(dateLibrary);

        cliOptions.add(CliOption.newBoolean(
                Options.SUBPROJECT,
                "Generates a gradle subproject (without wrapper and settings)",
                Options.Defaults.SUBPROJECT
        ));

        cliOptions.add(newPlatformOption(Options.Platform.JVM, Options.Defaults.JVM_ENABLED));
        // Disabled due to incomplete build
        //cliOptions.add(newPlatformOption(Options.Platform.ANDROID, Options.Defaults.ANDROID_ENABLED));
        cliOptions.add(newPlatformOption(Options.Platform.JS, Options.Defaults.JS_ENABLED));
        cliOptions.add(newPlatformOption(Options.Platform.IOS, Options.Defaults.IOS_ENABLED));
        cliOptions.add(newPlatformOption(Options.Platform.NATIVE, Options.Defaults.NATIVE_ENABLED));

        cliOptions.add(CliOption.newBoolean(
                Options.JS_BROWSER,
                "Add browser support to js module",
                Options.Defaults.JS_BROWSER
        ));
        cliOptions.add(CliOption.newBoolean(
                Options.JS_NODE,
                "Add nodejs support to js module",
                Options.Defaults.JS_NODE
        ));

        CliOption jvmAsync = new CliOption(Options.JVM_ASYNC, "Option. Date library to use");
        Map<String, String> jvmAsyncOptions = new HashMap<>();
        jvmAsyncOptions.put(Options.JavaAsync.NONE.value, "Only supports coroutines on jvm");
        jvmAsyncOptions.put(Options.JavaAsync.JDK8.value, "Adds additional support for java 8 `CompletableFuture` on jvm");
        jvmAsync.setEnum(jvmAsyncOptions);
        jvmAsync.setDefault(Options.Defaults.DATE_LIBRARY.value);
        cliOptions.add(jvmAsync);

        // Disabled due to incomplete build
        //CliOption androidAsync = new CliOption(Options.ANDROID_ASYNC, "Option. Date library to use");
        //Map<String, String> androidAsyncOptions = new HashMap<>();
        //androidAsyncOptions.put(Options.JavaAsync.NONE.value, "Only supports coroutines on android");
        //androidAsyncOptions.put(Options.JavaAsync.JDK8.value, "Adds additional support for java 8 `CompletableFuture` on android");
        //androidAsync.setEnum(androidAsyncOptions);
        //androidAsync.setDefault(Options.Defaults.ANDROID_ASYNC.value);
        //cliOptions.add(androidAsync);

        CliOption jsAsync = new CliOption(Options.JS_ASYNC, "Option. Date library to use");
        Map<String, String> jsAsyncOptions = new HashMap<>();
        jsAsyncOptions.put(Options.JsAsync.NONE.value, "Only supports coroutines in javascript");
        jsAsyncOptions.put(Options.JsAsync.PROMISE.value, "Adds additional support for Promises in javascript");
        jsAsync.setEnum(jsAsyncOptions);
        jsAsync.setDefault(Options.Defaults.JS_ASYNC.value);
        cliOptions.add(jsAsync);
    }

    @Override
    public void processOpts() {
        for (String excluded : optionExcludes) {
            additionalProperties.remove(excluded);
        }

        super.processOpts();

        String kotlinVersion = stringOption(Options.KOTLIN_VERSION, Options.Defaults.KOTLIN_VERSION);
        String ktorVersion = stringOption(Options.KTOR_VERSION, Options.Defaults.KTOR_VERSION);
        String gradleVersion = stringOption(Options.GRADLE_VERSION, Options.Defaults.GRADLE_VERSION);
        String androidGradleVersion = stringOption(Options.ANDROID_GRADLE_VERSION, Options.Defaults.ANDROID_GRADLE_VERSION);
        String kotlinxDatetimeVersion = stringOption(Options.KOTLINX_DATETIME_VERSION, Options.Defaults.KOTLINX_DATETIME_VERSION);

        Options.DateLibrary dateLibrary = Options.DateLibrary.fromName(stringOption(Options.DATE_LIBRARY, Options.Defaults.DATE_LIBRARY.value));
        setDateLibrary(dateLibrary);
        boolean subproject = booleanOption(Options.SUBPROJECT, Options.Defaults.SUBPROJECT);

        boolean jvmEnabled = platformOption(Options.Platform.JVM, Options.Defaults.JVM_ENABLED);
        boolean androidEnabled = platformOption(Options.Platform.ANDROID, Options.Defaults.ANDROID_ENABLED);
        boolean jsEnabled = platformOption(Options.Platform.JS, Options.Defaults.JS_ENABLED);
        boolean iosEnabled = platformOption(Options.Platform.IOS, Options.Defaults.IOS_ENABLED);
        boolean nativeEnabled = platformOption(Options.Platform.NATIVE, Options.Defaults.NATIVE_ENABLED);

        Options.JavaAsync jvmAsync = Options.JavaAsync.fromName(stringOption(Options.JVM_ASYNC, Options.Defaults.JVM_ASYNC.value));
        setJvmAsync(jvmAsync);

        Options.JavaAsync androidAsync = Options.JavaAsync.fromName(stringOption(Options.ANDROID_ASYNC, Options.Defaults.ANDROID_ASYNC.value));
        setAndroidAsync(androidAsync);

        Options.JsAsync jsAsync = Options.JsAsync.fromName(stringOption(Options.JS_ASYNC, Options.Defaults.JS_ASYNC.value));
        setJsAsync(jsAsync);

        boolean jsBrowser = booleanOption(Options.JS_BROWSER, Options.Defaults.JS_BROWSER);
        boolean jsNode = booleanOption(Options.JS_NODE, Options.Defaults.JS_NODE);

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        // Build
        supportingFiles.add(new SupportingFile("build.gradle.kts.mustache", "", "build.gradle.kts"));
        if (!subproject) {
            supportingFiles.add(new SupportingFile("settings.gradle.kts.mustache", "", "settings.gradle.kts"));
            supportingFiles.add(new SupportingFile("gradlew", "", "gradlew"));
            supportingFiles.add(new SupportingFile("gradlew.bat", "", "gradlew.bat"));
            String wrapperSrc = "gradle/wrapper";
            String wrapperDest = "gradle" + File.separator + "wrapper";
            supportingFiles.add(new SupportingFile(wrapperSrc + File.separator + "gradle-wrapper.properties.mustache", wrapperDest, "gradle-wrapper.properties"));
            supportingFiles.add(new SupportingFile(wrapperSrc + File.separator + "gradle-wrapper.jar", wrapperDest, "gradle-wrapper.jar"));
        }

        final String srcDir = sourceFolder + File.separator + packageName;

        // Infra
        final String dest = srcDir.replace(".", File.separator);
        final String infraDest = (srcDir + File.separator + "infrastructure").replace(".", File.separator);
        final String infraSrc = "common/main/infrastructure/";
        supportingFiles.add(new SupportingFile("ApiClient.kt.mustache", dest, "ApiClient.kt"));
        supportingFiles.add(new SupportingFile(infraSrc + "ApiClientBase.kt.mustache", infraDest, "ApiClientBase.kt"));
        supportingFiles.add(new SupportingFile(infraSrc + "ApiAbstractions.kt.mustache", infraDest, "ApiAbstractions.kt"));
        supportingFiles.add(new SupportingFile(infraSrc + "RequestConfig.kt.mustache", infraDest, "RequestConfig.kt"));
        supportingFiles.add(new SupportingFile(infraSrc + "RequestMethod.kt.mustache", infraDest, "RequestMethod.kt"));
        supportingFiles.add(new SupportingFile(infraSrc + "Base64ByteArray.kt.mustache", infraDest, "Base64ByteArray.kt"));
        supportingFiles.add(new SupportingFile(infraSrc + "Bytes.kt.mustache", infraDest, "Bytes.kt"));
        supportingFiles.add(new SupportingFile(infraSrc + "HttpResponse.kt.mustache", infraDest, "HttpResponse.kt"));
        supportingFiles.add(new SupportingFile(infraSrc + "OctetByteArray.kt.mustache", infraDest, "OctetByteArray.kt"));
        if (dateLibrary == Options.DateLibrary.KOTLINX) {
            supportingFiles.add(new SupportingFile(infraSrc + "dateTime.kt.mustache", infraDest, "dateTime.kt"));
        }

        // Auth
        final String authDest = (srcDir + File.separator + "auth").replace(".", File.separator);
        final String authSrc = "common/main/auth/";
        supportingFiles.add(new SupportingFile(authSrc + "ApiKeyAuth.kt.mustache", authDest, "ApiKeyAuth.kt"));
        supportingFiles.add(new SupportingFile(authSrc + "Authentication.kt.mustache", authDest, "Authentication.kt"));
        supportingFiles.add(new SupportingFile(authSrc + "HttpBasicAuth.kt.mustache", authDest, "HttpBasicAuth.kt"));
        supportingFiles.add(new SupportingFile(authSrc + "HttpBearerAuth.kt.mustache", authDest, "HttpBearerAuth.kt"));
        supportingFiles.add(new SupportingFile(authSrc + "OAuth.kt.mustache", authDest, "OAuth.kt"));

        // Multiplatform default includes
        defaultIncludes.add("io.ktor.client.request.forms.InputProvider");
        defaultIncludes.add("io.ktor.client.statement.HttpResponse");
        defaultIncludes.add(packageName + ".infrastructure.Base64ByteArray");
        defaultIncludes.add(packageName + ".infrastructure.OctetByteArray");

        // Multiplatform type mapping
        typeMapping.put("number", "kotlin.Double");
        typeMapping.put("file", "OctetByteArray");
        typeMapping.put("binary", "OctetByteArray");
        typeMapping.put("ByteArray", "Base64ByteArray");
        typeMapping.put("object", "kotlin.String");  // kotlin.Any not serializable

        // Multiplatform import mapping
        importMapping.put("BigDecimal", "kotlin.Double");
        importMapping.put("UUID", "kotlin.String");
        importMapping.put("URI", "kotlin.String");
        importMapping.put("InputProvider", "io.ktor.client.request.forms.InputProvider");
        importMapping.put("File", "io.ktor.client.statement.HttpResponse");
        importMapping.put("Base64ByteArray", packageName + ".infrastructure.Base64ByteArray");
        importMapping.put("OctetByteArray", packageName + ".infrastructure.OctetByteArray");

        switch (dateLibrary) {
            case STRING: {
                typeMapping.put("date-time", "kotlin.String");
                typeMapping.put("date", "kotlin.String");
                typeMapping.put("Date", "kotlin.String");
                typeMapping.put("DateTime", "kotlin.String");
                importMapping.put("Timestamp", "kotlin.String");
                importMapping.put("LocalDateTime", "kotlin.String");
                importMapping.put("LocalDate", "kotlin.String");
                importMapping.put("LocalTime", "kotlin.String");
            }
            case KOTLINX: {
                typeMapping.put("date-time", "LocalDateTime");
                typeMapping.put("date", "LocalDate");
                typeMapping.put("Date", "LocalDate");
                typeMapping.put("DateTime", "LocalDateTime");
                importMapping.put("Timestamp", "kotlinx.datetime.Instant");
                importMapping.put("LocalDateTime", "kotlinx.datetime.LocalDateTime");
                importMapping.put("LocalDate", "kotlinx.datetime.LocalDate");
                importMapping.put("LocalTime", "kotlin.String");
                defaultIncludes.add("kotlinx.datetime.Instant");
                defaultIncludes.add("kotlinx.datetime.LocalDateTime");
                defaultIncludes.add("kotlinx.datetime.LocalDate");
            }
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        Map<String, Object> objects = super.postProcessModels(objs);
        @SuppressWarnings("unchecked") List<Object> models = (List<Object>) objs.get("models");

        for (Object model : models) {
            @SuppressWarnings("unchecked") Map<String, Object> mo = (Map<String, Object>) model;
            CodegenModel cm = (CodegenModel) mo.get("model");

            // Escape the variable base name for use as a string literal
            List<CodegenProperty> vars = Stream.of(
                    cm.vars,
                    cm.allVars,
                    cm.optionalVars,
                    cm.requiredVars,
                    cm.readOnlyVars,
                    cm.readWriteVars,
                    cm.parentVars
            )
                    .flatMap(List::stream)
                    .collect(Collectors.toList());

            for (CodegenProperty var : vars) {
                var.vendorExtensions.put(VENDOR_EXTENSION_BASE_NAME_LITERAL, var.baseName.replace("$", "\\$"));
            }
        }

        return objects;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                // Set multipart against all relevant operations
                if (operation.hasConsumes == Boolean.TRUE) {
                    if (isMultipartType(operation.consumes)) {
                        operation.isMultipart = Boolean.TRUE;
                    }
                }

                // Modify the data type of binary form parameters to a more friendly type for multiplatform builds
                for (CodegenParameter param : operation.allParams) {
                    if (param.dataFormat != null && param.dataFormat.equals("binary")) {
                        param.baseType = param.dataType = "io.ktor.client.request.forms.InputProvider";
                    }
                }
            }
        }
        return operations;
    }

    private static boolean isMultipartType(List<Map<String, String>> consumes) {
        Map<String, String> firstType = consumes.get(0);
        if (firstType != null) {
            return "multipart/form-data".equals(firstType.get("mediaType"));
        }
        return false;
    }

    public boolean platformOption(Options.Platform platform, boolean defaultValue) {
        String option = platform.value + "Enabled";
        if (additionalProperties.containsKey(option)) {
            return (boolean) additionalProperties.get(option);
        }
        additionalProperties.put(option, defaultValue);
        return defaultValue;
    }

    private String stringOption(String option, String defaultValue) {
        if (additionalProperties.containsKey(option)) {
            return (String) additionalProperties.get(option);
        }
        additionalProperties.put(option, defaultValue);
        return defaultValue;
    }

    private boolean booleanOption(String option, boolean defaultValue) {
        if (additionalProperties.containsKey(option)) {
            return (boolean) additionalProperties.get(option);
        }
        additionalProperties.put(option, defaultValue);
        return defaultValue;
    }

    private void setDateLibrary(Options.DateLibrary dateLibrary) {
        switch (dateLibrary) {
            case STRING: {
                additionalProperties.put("dateLibraryString", true);
                additionalProperties.put("dateLibraryKotlinx", false);
                break;
            }
            case KOTLINX: {
                additionalProperties.put("dateLibraryKotlinx", true);
                additionalProperties.put("dateLibraryString", false);
                break;
            }
            default: {
                throw new IllegalStateException();
            }
        }
    }

    private void setJsAsync(Options.JsAsync jsAsync) {
        switch (jsAsync) {
            case NONE: {
                additionalProperties.put("jsAsyncPromise", false);
                break;
            }
            case PROMISE: {
                additionalProperties.put("jsAsyncPromise", true);
                break;
            }
            default: {
                throw new IllegalStateException();
            }
        }
    }

    private void setAndroidAsync(Options.JavaAsync androidAsync) {
        switch (androidAsync) {
            case NONE: {
                additionalProperties.put("androidAsyncJdk8", false);
                break;
            }
            case JDK8: {
                additionalProperties.put("androidAsyncJdk8", true);
                break;
            }
            default: {
                throw new IllegalStateException();
            }
        }
    }

    private void setJvmAsync(Options.JavaAsync jvmAsync) {
        switch (jvmAsync) {
            case NONE: {
                additionalProperties.put("jvmAsyncJdk8", false);
                break;
            }
            case JDK8: {
                additionalProperties.put("jvmAsyncJdk8", true);
                break;
            }
            default: {
                throw new IllegalStateException();
            }
        }
    }

    private static class InvalidEnumNameException extends IllegalArgumentException {
        InvalidEnumNameException(String name, String enumName, Stream<String> values) {
            super("\"" + name + "\" isn't a valid option for enum " + enumName + "; Options: " + values.map(v -> "\"" + v + "\"").collect(Collectors.joining(", ")));
        }
    }
}
