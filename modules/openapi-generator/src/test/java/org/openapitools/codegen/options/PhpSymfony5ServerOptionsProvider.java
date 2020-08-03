package org.openapitools.codegen.options;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.AbstractPhpCodegen;
import org.openapitools.codegen.languages.PhpSymfonyServerCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class PhpSymfony5ServerOptionsProvider implements OptionsProvider {
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String VARIABLE_NAMING_CONVENTION_VALUE = "camelCase";
    public static final String INVOKER_PACKAGE_VALUE = "OpenAPIServer";
    public static final String PACKAGE_NAME_VALUE = "";
    public static final String SRC_BASE_PATH_VALUE = "src";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String HIDE_GENERATION_TIMESTAMP_VALUE = "true";
    public static final String BUNDLE_NAME_VALUE = "OpenAPIServer";
    public static final String BUNDLE_ALIAS_VALUE = "open_api_server";
    public static final String COMPOSER_VENDOR_NAME_VALUE = "openapi";
    public static final String COMPOSER_PROJECT_NAME_VALUE = "server-bundle";
    public static final String PHP_LEGACY_SUPPORT_VALUE = "true";

    @Override
    public String getLanguage() {
        return "php-symfony5";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(AbstractPhpCodegen.VARIABLE_NAMING_CONVENTION, VARIABLE_NAMING_CONVENTION_VALUE)
                .put(AbstractPhpCodegen.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(AbstractPhpCodegen.SRC_BASE_PATH, SRC_BASE_PATH_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "true")
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, "true")
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, HIDE_GENERATION_TIMESTAMP_VALUE)
                .put(PhpSymfonyServerCodegen.BUNDLE_NAME, BUNDLE_NAME_VALUE)
                .put(PhpSymfonyServerCodegen.BUNDLE_ALIAS, BUNDLE_ALIAS_VALUE)
                .put(PhpSymfonyServerCodegen.COMPOSER_VENDOR_NAME, COMPOSER_VENDOR_NAME_VALUE)
                .put(PhpSymfonyServerCodegen.COMPOSER_PROJECT_NAME, COMPOSER_PROJECT_NAME_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}

