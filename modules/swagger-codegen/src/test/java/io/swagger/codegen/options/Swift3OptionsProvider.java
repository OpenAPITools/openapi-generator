package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.Swift3Codegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class Swift3OptionsProvider implements OptionsProvider {
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String PROJECT_NAME_VALUE = "Swagger";
    public static final String RESPONSE_AS_VALUE = "test";
    public static final String UNWRAP_REQUIRED_VALUE = "true";
    public static final String POD_SOURCE_VALUE = "{ :git => 'git@github.com:swagger-api/swagger-mustache.git'," +
            " :tag => 'v1.0.0-SNAPSHOT' }";
    public static final String POD_VERSION_VALUE = "v1.0.0-SNAPSHOT";
    public static final String POD_AUTHORS_VALUE = "podAuthors";
    public static final String POD_SOCIAL_MEDIA_URL_VALUE = "podSocialMediaURL";
    public static final String POD_DOCSET_URL_VALUE = "podDocsetURL";
    public static final String POD_LICENSE_VALUE = "'Apache License, Version 2.0'";
    public static final String POD_HOMEPAGE_VALUE = "podHomepage";
    public static final String POD_SUMMARY_VALUE = "podSummary";
    public static final String POD_DESCRIPTION_VALUE = "podDescription";
    public static final String POD_SCREENSHOTS_VALUE = "podScreenshots";
    public static final String POD_DOCUMENTATION_URL_VALUE = "podDocumentationURL";
    public static final String SWIFT_USE_API_NAMESPACE_VALUE = "swiftUseApiNamespace";

    @Override
    public String getLanguage() {
        return "swift";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(Swift3Codegen.PROJECT_NAME, PROJECT_NAME_VALUE)
                .put(Swift3Codegen.RESPONSE_AS, RESPONSE_AS_VALUE)
                .put(Swift3Codegen.UNWRAP_REQUIRED, UNWRAP_REQUIRED_VALUE)
                .put(Swift3Codegen.POD_SOURCE, POD_SOURCE_VALUE)
                .put(CodegenConstants.POD_VERSION, POD_VERSION_VALUE)
                .put(Swift3Codegen.POD_AUTHORS, POD_AUTHORS_VALUE)
                .put(Swift3Codegen.POD_SOCIAL_MEDIA_URL, POD_SOCIAL_MEDIA_URL_VALUE)
                .put(Swift3Codegen.POD_DOCSET_URL, POD_DOCSET_URL_VALUE)
                .put(Swift3Codegen.POD_LICENSE, POD_LICENSE_VALUE)
                .put(Swift3Codegen.POD_HOMEPAGE, POD_HOMEPAGE_VALUE)
                .put(Swift3Codegen.POD_SUMMARY, POD_SUMMARY_VALUE)
                .put(Swift3Codegen.POD_DESCRIPTION, POD_DESCRIPTION_VALUE)
                .put(Swift3Codegen.POD_SCREENSHOTS, POD_SCREENSHOTS_VALUE)
                .put(Swift3Codegen.POD_DOCUMENTATION_URL, POD_DOCUMENTATION_URL_VALUE)
                .put(Swift3Codegen.SWIFT_USE_API_NAMESPACE, SWIFT_USE_API_NAMESPACE_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
