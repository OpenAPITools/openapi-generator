package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.ObjcClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class ObjcClientOptionsProvider implements OptionsProvider {
    public static final String CLASS_PREFIX_VALUE = "SWGObjc";
    public static final String CORE_DATA_VALUE = "n";
    public static final String POD_NAME_VALUE = "SwaggerClientObjc";
    public static final String POD_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String AUTHOR_NAME_VALUE = "SwaggerObjc";
    public static final String AUTHOR_EMAIL_VALUE = "objc@swagger.io";
    public static final String GIT_REPO_URL_VALUE = "https://github.com/swagger-api/swagger-codegen";

    @Override
    public String getLanguage() {
        return "objc";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(ObjcClientCodegen.CLASS_PREFIX, CLASS_PREFIX_VALUE)
                .put(ObjcClientCodegen.POD_NAME, POD_NAME_VALUE)
                .put(CodegenConstants.POD_VERSION, POD_VERSION_VALUE)
                .put(ObjcClientCodegen.AUTHOR_NAME, AUTHOR_NAME_VALUE)
                .put(ObjcClientCodegen.AUTHOR_EMAIL, AUTHOR_EMAIL_VALUE)
                .put(ObjcClientCodegen.GIT_REPO_URL, GIT_REPO_URL_VALUE)
                .put(ObjcClientCodegen.CORE_DATA, CORE_DATA_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
