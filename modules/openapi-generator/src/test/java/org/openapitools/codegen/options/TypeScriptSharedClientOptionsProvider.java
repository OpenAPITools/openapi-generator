package org.openapitools.codegen.options;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.AbstractTypeScriptClientCodegen;

import java.util.Map;

import static java.util.Map.entry;

public interface TypeScriptSharedClientOptionsProvider extends OptionsProvider {
    String NPM_NAME_VALUE = "npmName";
    String NPM_VERSION_VALUE = "1.1.2";
    String SUPPORTS_ES6_VALUE = "false";
    String NULL_SAFE_ADDITIONAL_PROPS_VALUE = "false";
    String ENUM_NAME_SUFFIX_VALUE = "Enum";
    String ENUM_PROPERTY_NAMING_VALUE = "PascalCase";
    String MODEL_PROPERTY_NAMING_VALUE = "camelCase";
    String PARAM_NAMING_VALUE = "camelCase";
    String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";
    String ENUM_UNKNOWN_DEFAULT_CASE_VALUE = "false";
    String ENUM_PROPERTY_NAMING_REPLACE_SPECIAL_CHAR_VALUE = "false";
    String SORT_PARAMS_VALUE = "false";
    String SORT_MODEL_PROPERTIES_VALUE = "false";
    String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    String LEGACY_DISCRIMINATOR_BEHAVIOUR_VALUE = "true";
    String DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT_VALUE = "true";
    String SNAPSHOT_VALUE = Boolean.FALSE.toString();
    String LICENCE_NAME_VALUE = "Unlicense";

    @Override
    default boolean isServer() {
        return false;
    }

    @Override
    default Map<String, String> createOptions() {
        return Map.ofEntries(
                entry(AbstractTypeScriptClientCodegen.NPM_NAME, NPM_NAME_VALUE),
                entry(AbstractTypeScriptClientCodegen.NPM_VERSION, NPM_VERSION_VALUE),
                entry(CodegenConstants.SUPPORTS_ES6, SUPPORTS_ES6_VALUE),
                entry(AbstractTypeScriptClientCodegen.NULL_SAFE_ADDITIONAL_PROPS, NULL_SAFE_ADDITIONAL_PROPS_VALUE),
                entry(CodegenConstants.ENUM_NAME_SUFFIX, ENUM_NAME_SUFFIX_VALUE),
                entry(CodegenConstants.ENUM_PROPERTY_NAMING, ENUM_PROPERTY_NAMING_VALUE),
                entry(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING_VALUE),
                entry(CodegenConstants.PARAM_NAMING, PARAM_NAMING_VALUE),
                entry(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE),
                entry(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE),
                entry(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, ENUM_UNKNOWN_DEFAULT_CASE_VALUE),
                entry(AbstractTypeScriptClientCodegen.ENUM_PROPERTY_NAMING_REPLACE_SPECIAL_CHAR, ENUM_PROPERTY_NAMING_REPLACE_SPECIAL_CHAR_VALUE),
                entry(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE),
                entry(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, SORT_MODEL_PROPERTIES_VALUE),
                entry(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE),
                entry(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, LEGACY_DISCRIMINATOR_BEHAVIOUR_VALUE),
                entry(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT, DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT_VALUE),
                entry(AbstractTypeScriptClientCodegen.SNAPSHOT, SNAPSHOT_VALUE),
                entry(CodegenConstants.LICENSE_NAME, LICENCE_NAME_VALUE)
        );
    }
}
