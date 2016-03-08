package io.swagger.codegen.options;

import io.swagger.codegen.Codegen;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class JavaOptionsProvider implements OptionsProvider {
    public static final String ARTIFACT_ID_VALUE = "swagger-java-client-test";
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String INVOKER_PACKAGE_VALUE = "io.swagger.client.test";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String GROUP_ID_VALUE = "io.swagger.test";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SOURCE_FOLDER_VALUE = "src/main/java/test";
    public static final String LOCAL_PREFIX_VALUE = "tst";
    public static final String DEFAULT_LIBRARY_VALUE = "jersey2";
    public static final String SERIALIZABLE_MODEL_VALUE = "false";
    public static final String FULL_JAVA_UTIL_VALUE = "true";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";

    private ImmutableMap<String, String> options;

    /**
     * Create an options provider with the default options.
     */
    public JavaOptionsProvider() {
        options = new ImmutableMap.Builder<String, String>()
                .put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(CodegenConstants.GROUP_ID, GROUP_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_ID, ARTIFACT_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(CodegenConstants.LOCAL_VARIABLE_PREFIX, LOCAL_PREFIX_VALUE)
                .put(CodegenConstants.SERIALIZABLE_MODEL, SERIALIZABLE_MODEL_VALUE)
                .put(JavaClientCodegen.FULL_JAVA_UTIL, FULL_JAVA_UTIL_VALUE)
                .put(CodegenConstants.LIBRARY, DEFAULT_LIBRARY_VALUE)
                .put(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING, "true")
                .put(JavaClientCodegen.USE_RX_JAVA, "false")
                .put(JavaClientCodegen.DATE_LIBRARY, "joda")
                .build();
    }

    /**
     * Use the default options, but override the ones found in additionalOptions.
     */
    public JavaOptionsProvider(Map<String, String> additionalOptions) {
        options = new ImmutableMap.Builder<String, String>()
                .putAll(options)
                .putAll(additionalOptions)
                .build();
    }

    @Override
    public Map<String, String> createOptions() {
        return options;
    }

    @Override
    public boolean isServer() {
        return false;
    }

    @Override
    public String getLanguage() {
        return "java";
    }
}
