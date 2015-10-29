package io.swagger.codegen.swift;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.SwiftCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class SwiftOptionsTest extends AbstractOptionsTest {
    protected static final String MODEL_PACKAGE_VALUE = "package";
    protected static final String API_PACKAGE_VALUE = "apiPackage";
    protected static final String SORT_PARAMS_VALUE = "false";
    protected static final String PROJECT_NAME_VALUE = "Swagger";
    protected static final String RESPONSE_AS_VALUE = "test";
    protected static final String UNWRAP_REQUIRED_VALUE = "true";
    protected static final String POD_SOURCE_VALUE = "{ :git => 'git@github.com:swagger-api/swagger-mustache.git'," +
            " :tag => 'v1.0.0-SNAPSHOT' }";
    protected static final String POD_VERSION_VALUE = "v1.0.0-SNAPSHOT";
    protected static final String POD_AUTHORS_VALUE = "podAuthors";
    protected static final String POD_SOCIAL_MEDIA_URL_VALUE = "podSocialMediaURL";
    protected static final String POD_DOCSET_URL_VALUE = "podDocsetURL";
    protected static final String POD_LICENSE_VALUE = "'Apache License, Version 2.0'";
    protected static final String POD_HOMEPAGE_VALUE = "podHomepage";
    protected static final String POD_SUMMARY_VALUE = "podSummary";
    protected static final String POD_DESCRIPTION_VALUE = "podDescription";
    protected static final String POD_SCREENSHOTS_VALUE = "podScreenshots";
    protected static final String POD_DOCUMENTATION_URL_VALUE = "podDocumentationURL";

    @Tested
    private SwiftCodegen clientCodegen;

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setProjectName(PROJECT_NAME_VALUE);
            times = 1;
            clientCodegen.setResponseAs(RESPONSE_AS_VALUE.split(","));
            times = 1;
            clientCodegen.setUnwrapRequired(Boolean.valueOf(UNWRAP_REQUIRED_VALUE));
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(SwiftCodegen.PROJECT_NAME, PROJECT_NAME_VALUE)
                .put(SwiftCodegen.RESPONSE_AS, RESPONSE_AS_VALUE)
                .put(SwiftCodegen.UNWRAP_REQUIRED, UNWRAP_REQUIRED_VALUE)
                .put(SwiftCodegen.POD_SOURCE, POD_SOURCE_VALUE)
                .put(CodegenConstants.POD_VERSION, POD_VERSION_VALUE)
                .put(SwiftCodegen.POD_AUTHORS, POD_AUTHORS_VALUE)
                .put(SwiftCodegen.POD_SOCIAL_MEDIA_URL, POD_SOCIAL_MEDIA_URL_VALUE)
                .put(SwiftCodegen.POD_DOCSET_URL, POD_DOCSET_URL_VALUE)
                .put(SwiftCodegen.POD_LICENSE, POD_LICENSE_VALUE)
                .put(SwiftCodegen.POD_HOMEPAGE, POD_HOMEPAGE_VALUE)
                .put(SwiftCodegen.POD_SUMMARY, POD_SUMMARY_VALUE)
                .put(SwiftCodegen.POD_DESCRIPTION, POD_DESCRIPTION_VALUE)
                .put(SwiftCodegen.POD_SCREENSHOTS, POD_SCREENSHOTS_VALUE)
                .put(SwiftCodegen.POD_DOCUMENTATION_URL, POD_DOCUMENTATION_URL_VALUE)
                .build();
    }
}
