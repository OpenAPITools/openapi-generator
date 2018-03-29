package org.openapitools.codegen.options;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.GoClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class GoClientOptionsProvider implements OptionsProvider {

    public static final String PACKAGE_VERSION_VALUE = "1.0.0";
    public static final String PACKAGE_NAME_VALUE = "Go";
    public static final boolean WITH_XML_VALUE = true;
    public static final Boolean PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = true;

    @Override
    public String getLanguage() {
        return "go";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")
                .put(CodegenConstants.WITH_XML, "true")
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, "true")
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
