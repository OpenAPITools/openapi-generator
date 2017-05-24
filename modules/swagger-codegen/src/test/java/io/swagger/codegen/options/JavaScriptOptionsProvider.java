package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.options.OptionsProvider;
import io.swagger.codegen.languages.JavascriptClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class JavaScriptOptionsProvider implements OptionsProvider {
    public static final String ARTIFACT_ID_VALUE = "swagger-javascript-client-test";
    public static final String INVOKER_PACKAGE_VALUE = "invoker";
    public static final String MODEL_PACKAGE_VALUE = "model";
    public static final String API_PACKAGE_VALUE = "api";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String GROUP_ID_VALUE = "io.swagger.test";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SOURCE_FOLDER_VALUE = "src/main/javascript";
    public static final String LOCAL_PREFIX_VALUE = "_";
//    public static final String SERIALIZABLE_MODEL_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String PROJECT_NAME_VALUE = "JavaScript Client Test";
    public static final String MODULE_NAME_VALUE = "JavaScriptClient";
    public static final String PROJECT_DESCRIPTION_VALUE = "Tests JavaScript code generator options";
    public static final String PROJECT_VERSION_VALUE = "1.0.0";
    public static final String PROJECT_LICENSE_NAME_VALUE = "Apache";
    public static final String USE_PROMISES_VALUE = "true";
    public static final String USE_INHERITANCE_VALUE = "false";
    public static final String EMIT_MODEL_METHODS_VALUE = "true";
    public static final String EMIT_JS_DOC_VALUE = "false";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String USE_ES6_VALUE = "true";


    private ImmutableMap<String, String> options;

    /**
     * Create an options provider with the default options.
     */
    public JavaScriptOptionsProvider() {
        // Commented generic options not yet supported by JavaScript codegen.
        options = new ImmutableMap.Builder<String, String>()
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
//                .put(CodegenConstants.GROUP_ID, GROUP_ID_VALUE)
//                .put(CodegenConstants.ARTIFACT_ID, ARTIFACT_ID_VALUE)
//                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(CodegenConstants.LOCAL_VARIABLE_PREFIX, LOCAL_PREFIX_VALUE)
//                .put(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING, "true")
                .put(JavascriptClientCodegen.PROJECT_NAME, PROJECT_NAME_VALUE)
                .put(JavascriptClientCodegen.MODULE_NAME, MODULE_NAME_VALUE)
                .put(JavascriptClientCodegen.PROJECT_DESCRIPTION, PROJECT_DESCRIPTION_VALUE)
                .put(JavascriptClientCodegen.PROJECT_VERSION, PROJECT_VERSION_VALUE)
                .put(JavascriptClientCodegen.PROJECT_LICENSE_NAME, PROJECT_LICENSE_NAME_VALUE)
                .put(JavascriptClientCodegen.USE_PROMISES, USE_PROMISES_VALUE)
                .put(JavascriptClientCodegen.USE_INHERITANCE, USE_INHERITANCE_VALUE)
                .put(JavascriptClientCodegen.EMIT_MODEL_METHODS, EMIT_MODEL_METHODS_VALUE)
                .put(JavascriptClientCodegen.EMIT_JS_DOC, EMIT_JS_DOC_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(JavascriptClientCodegen.USE_ES6, USE_ES6_VALUE)
                .build();
    }

    /**
     * Use the default options, but override the ones found in additionalOptions.
     */
    public JavaScriptOptionsProvider(Map<String, String> additionalOptions) {
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
        return "javascript";
    }
}
