package org.openapitools.codegen.options;

import com.google.common.collect.ImmutableMap;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.GdscriptClientCodegen;

import java.util.Map;

public class GdscriptClientOptionsProvider implements OptionsProvider {

//    public static final String MODEL_PACKAGE_VALUE = "package";
//    public static final String API_PACKAGE_VALUE = "apiPackage";
//    public static final String VARIABLE_NAMING_CONVENTION_VALUE = "snake_case";
//    public static final String INVOKER_PACKAGE_VALUE = "OpenAPITools\\Client\\Php";
//    public static final String PACKAGE_NAME_VALUE = "OpenAPIToolsClient-php";
//    public static final String SRC_BASE_PATH_VALUE = "libPhp";
//    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";

    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    public static final String LEGACY_DISCRIMINATOR_BEHAVIOR_VALUE = "true";
    public static final String NO_ADDITIONAL_PROPERTIES_VALUE = "true";
    public static final String ENUM_UNKNOWN_DEFAULT_CASE_VALUE = "false";

    @Override
    public String getLanguage() {
        return "gdscript";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder

                .put(GdscriptClientCodegen.CORE_NAME_PREFIX, GdscriptClientCodegen.CORE_NAME_PREFIX_VALUE)
                .put(GdscriptClientCodegen.CORE_NAME_SUFFIX, GdscriptClientCodegen.CORE_NAME_SUFFIX_VALUE)
                .put(GdscriptClientCodegen.ANTICOLLISION_PREFIX, GdscriptClientCodegen.ANTICOLLISION_PREFIX_VALUE)
                .put(GdscriptClientCodegen.ANTICOLLISION_SUFFIX, GdscriptClientCodegen.ANTICOLLISION_SUFFIX_VALUE)

                // Things we *might* need (we'll see)

//                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
//                .put(PhpClientCodegen.VARIABLE_NAMING_CONVENTION, VARIABLE_NAMING_CONVENTION_VALUE)
//                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
//                .put(PhpClientCodegen.PACKAGE_NAME, PACKAGE_NAME_VALUE)
//                .put(PhpClientCodegen.SRC_BASE_PATH, SRC_BASE_PATH_VALUE)
//                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
//                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true")

                // The following is required by CodeGen

                //.put(CodegenConstants.TEMPLATING_ENGINE, "handlebars")
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .put(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, LEGACY_DISCRIMINATOR_BEHAVIOR_VALUE)
                .put(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, NO_ADDITIONAL_PROPERTIES_VALUE)
                .put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, ENUM_UNKNOWN_DEFAULT_CASE_VALUE)

                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}

