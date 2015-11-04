package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.AndroidVolleyClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class AndroidVolleyClientOptionsProvider implements OptionsProvider {
    public static final String ARTIFACT_ID_VALUE = "swagger-java-client-test";
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String INVOKER_PACKAGE_VALUE = "io.swagger.client.test";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String GROUP_ID_VALUE = "io.swagger.test";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SOURCE_FOLDER_VALUE = "src/main/java/test";
    public static final String ANDROID_MAVEN_GRADLE_PLUGIN_VALUE = "true";

    @Override
    public String getLanguage() {
        return "android-volley";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(CodegenConstants.GROUP_ID, GROUP_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_ID, ARTIFACT_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(AndroidVolleyClientCodegen.USE_ANDROID_MAVEN_GRADLE_PLUGIN, ANDROID_MAVEN_GRADLE_PLUGIN_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
