package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.openapitools.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class KotlinMultiplatformClientCodegen extends AbstractKotlinCodegen {
    public static final String PROJECT_NAME = "projectName";

    private static final Logger LOGGER = LoggerFactory.getLogger(KotlinMultiplatformClientCodegen.class);

    private static final String[] optionExcludes = new String[]{
            CodegenConstants.PARCELIZE_MODELS,
            CodegenConstants.SERIALIZABLE_MODEL,
            CodegenConstants.SERIALIZATION_LIBRARY
    };

    // Versions
    protected String kotlinVersion;
    protected String ktorVersion;
    protected String gradleVersion;

    // Platforms specific options
    protected boolean jvmEnabled;
    protected boolean androidEnabled;
    protected boolean jsEnabled;
    protected boolean iosEnabled;
    protected boolean nativeEnabled;

    protected boolean jsAsyncPromise;
    protected boolean jvmAsyncJdk8;
    protected boolean androidAsyncJdk8;

    // Other options
    protected boolean dateLibraryString;
    protected boolean dateLibraryIntegrated;
    protected boolean subproject;
    protected boolean jsBrowser;
    protected boolean jsNode;

    public static final class Options {
        // Versions
        public static final String KOTLIN_VERSION = "kotlinVersion";
        public static final String KTOR_VERSION = "ktorVersion";
        public static final String GRADLE_VERSION = "gradleVersion";

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
            public static final String KOTLIN_VERSION = "1.4.0";
            public static final String KTOR_VERSION = "1.4.0";
            public static final String GRADLE_VERSION = "6.6.1";

            // Platforms specific options
            // Default choices: https://www.jetbrains.com/lp/devecosystem-2020/kotlin/
            public static final boolean JVM_ENABLED = true;
            public static final boolean ANDROID_ENABLED = true;
            public static final boolean JS_ENABLED = false;
            public static final boolean IOS_ENABLED = false;
            public static final boolean NATIVE_ENABLED = false;

            public static final JavaAsync JVM_ASYNC = JavaAsync.JDK8;
            public static final JavaAsync ANDROID_ASYNC = JavaAsync.NONE;
            public static final JsAsync JS_ASYNC = JsAsync.PROMISE;
            public static final boolean JS_BROWSER = true;
            public static final boolean JS_NODE = false;

            // Other options
            public static final DateLibrary DATE_LIBRARY = DateLibrary.INTEGRATED;
            public static final boolean SUBPROJECT = false;
        }

        // Enums
        public enum Platform {
            JVM("jvm"),
            ANDROID("android"),
            JS("js"),
            IOS("ios"),
            NATIVE("native");

            public final String name;

            Platform(String name) {
                this.name = name;
            }

            public static Platform fromName(String name) {
                return Arrays.stream(values())
                        .filter(v -> v.name.equals(name))
                        .findAny()
                        .orElseThrow(IllegalArgumentException::new);
            }
        }

        public enum DateLibrary {
            STRING("string"),
            INTEGRATED("integrated");

            public final String name;

            DateLibrary(String name) {
                this.name = name;
            }

            public static DateLibrary fromName(String name) {
                return Arrays.stream(values())
                        .filter(v -> v.name.equals(name))
                        .findAny()
                        .orElseThrow(IllegalArgumentException::new);
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

            public final String name;

            JavaAsync(String name) {
                this.name = name;
            }

            public static JavaAsync fromName(String name) {
                return Arrays.stream(values())
                        .filter(v -> v.name.equals(name))
                        .findAny()
                        .orElseThrow(IllegalArgumentException::new);
            }
        }

        public enum JsAsync {
            NONE("none"),
            PROMISE("promise");

            public final String name;

            JsAsync(String name) {
                this.name = name;
            }

            public static JsAsync fromName(String name) {
                return Arrays.stream(values())
                        .filter(v -> v.name.equals(name))
                        .findAny()
                        .orElseThrow(IllegalArgumentException::new);
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

    public static CliOption platformOption(Options.Platform platform, boolean defaultState) {
        return CliOption.newBoolean(
                platform.name + "Enabled", "Enables generation of core for " + platform.name + " in the multiplatform project",
                defaultState
        );
    }

    public boolean getPlatformOption(Options.Platform platform) {
        return (boolean) additionalProperties.get(platform.name + "Enabled");
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
        embeddedTemplateDir = templateDir = "kotlin-client";
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        cliOptions.removeIf(option -> Arrays.stream(optionExcludes).anyMatch(e -> e.equals(option.getOpt())));

        cliOptions.add(CliOption.newString(Options.KOTLIN_VERSION, Options.Defaults.KOTLIN_VERSION));
        cliOptions.add(CliOption.newString(Options.KTOR_VERSION, Options.Defaults.KTOR_VERSION));
        cliOptions.add(CliOption.newString(Options.GRADLE_VERSION, Options.Defaults.GRADLE_VERSION));

        CliOption dateLibrary = new CliOption(Options.DATE_LIBRARY, "Option. Date library to use");
        Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(Options.DateLibrary.INTEGRATED.name, "Custom Date objects that may be converted into platform specific types via helpers from the `actual` objects");
        dateOptions.put(Options.DateLibrary.STRING.name, "String");
        dateLibrary.setEnum(dateOptions);
        dateLibrary.setDefault(Options.Defaults.DATE_LIBRARY.name);
        cliOptions.add(dateLibrary);

        cliOptions.add(CliOption.newBoolean(
                Options.SUBPROJECT,
                "Generates a gradle subproject (without wrapper and settings)",
                Options.Defaults.SUBPROJECT
        ));

        cliOptions.add(platformOption(Options.Platform.JVM, Options.Defaults.JVM_ENABLED));
        cliOptions.add(platformOption(Options.Platform.ANDROID, Options.Defaults.ANDROID_ENABLED));
        cliOptions.add(platformOption(Options.Platform.JS, Options.Defaults.JS_ENABLED));
        cliOptions.add(platformOption(Options.Platform.IOS, Options.Defaults.IOS_ENABLED));
        cliOptions.add(platformOption(Options.Platform.NATIVE, Options.Defaults.NATIVE_ENABLED));

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
        jvmAsyncOptions.put(Options.JavaAsync.NONE.name, "Only supports coroutines on jvm");
        jvmAsyncOptions.put(Options.JavaAsync.JDK8.name, "Adds additional support for java 8 `CompletableFuture` on jvm");
        jvmAsync.setEnum(jvmAsyncOptions);
        jvmAsync.setDefault(Options.Defaults.DATE_LIBRARY.name);
        cliOptions.add(jvmAsync);

        CliOption androidAsync = new CliOption(Options.ANDROID_ASYNC, "Option. Date library to use");
        Map<String, String> androidAsyncOptions = new HashMap<>();
        androidAsyncOptions.put(Options.JavaAsync.NONE.name, "Only supports coroutines on android");
        androidAsyncOptions.put(Options.JavaAsync.JDK8.name, "Adds additional support for java 8 `CompletableFuture` on android");
        androidAsync.setEnum(androidAsyncOptions);
        androidAsync.setDefault(Options.Defaults.ANDROID_ASYNC.name);
        cliOptions.add(androidAsync);

        CliOption jsAsync = new CliOption(Options.JS_ASYNC, "Option. Date library to use");
        Map<String, String> jsAsyncOptions = new HashMap<>();
        jsAsyncOptions.put(Options.JsAsync.NONE.name, "Only supports coroutines in javascript");
        jsAsyncOptions.put(Options.JsAsync.PROMISE.name, "Adds additional support for Promises in javascript");
        jsAsync.setEnum(jsAsyncOptions);
        jsAsync.setDefault(Options.Defaults.JS_ASYNC.name);
        cliOptions.add(jsAsync);
    }

    @Override
    public void processOpts() {
        for (String excluded : optionExcludes) {
            additionalProperties.remove(excluded);
        }

        super.processOpts();

        kotlinVersion = (String) additionalProperties.get(Options.KOTLIN_VERSION);
        ktorVersion = (String) additionalProperties.get(Options.KTOR_VERSION);
        gradleVersion = (String) additionalProperties.get(Options.GRADLE_VERSION);

        Options.DateLibrary dateLibrary = Options.DateLibrary.fromName((String) additionalProperties.get(Options.DATE_LIBRARY));
        setDateLibrary(dateLibrary);
        subproject = (boolean) additionalProperties.get(Options.SUBPROJECT);

        jvmEnabled = getPlatformOption(Options.Platform.JVM);
        androidEnabled = getPlatformOption(Options.Platform.ANDROID);
        jsEnabled = getPlatformOption(Options.Platform.JS);
        iosEnabled = getPlatformOption(Options.Platform.IOS);
        nativeEnabled = getPlatformOption(Options.Platform.NATIVE);

        Options.JavaAsync jvmAsync = Options.JavaAsync.fromName((String) additionalProperties.get(Options.JVM_ASYNC));
        setJvmAsync(jvmAsync);

        Options.JavaAsync androidAsync = Options.JavaAsync.fromName((String) additionalProperties.get(Options.ANDROID_ASYNC));
        setAndroidAsync(androidAsync);

        Options.JsAsync jsAsync = Options.JsAsync.fromName((String) additionalProperties.get(Options.JS_ASYNC));
        setJsAsync(jsAsync);

        jsBrowser = (boolean) additionalProperties.get(Options.JS_BROWSER);
        jsNode = (boolean) additionalProperties.get(Options.JS_NODE);
    }

    private void setDateLibrary(Options.DateLibrary dateLibrary) {
        switch (dateLibrary) {
            case STRING: {
                dateLibraryString = true;
                dateLibraryIntegrated = false;
                break;
            }
            case INTEGRATED: {
                dateLibraryIntegrated = true;
                dateLibraryString = false;
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
                jsAsyncPromise = false;
            }
            case PROMISE: {
                jsAsyncPromise = true;
            }
            default: {
                throw new IllegalStateException();
            }
        }
    }

    private void setAndroidAsync(Options.JavaAsync androidAsync) {
        switch (androidAsync) {
            case NONE: {
                androidAsyncJdk8 = false;
            }
            case JDK8: {
                androidAsyncJdk8 = true;
            }
        }
    }

    private void setJvmAsync(Options.JavaAsync jvmAsync) {
        switch (jvmAsync) {
            case NONE: {
                jvmAsyncJdk8 = false;
            }
            case JDK8: {
                jvmAsyncJdk8 = true;
            }
        }
    }
}
