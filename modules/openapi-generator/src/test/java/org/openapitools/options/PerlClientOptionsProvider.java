package org.openapitools.codegen.options;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.PerlClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class PerlClientOptionsProvider implements OptionsProvider {
    public static final String MODULE_NAME_VALUE = "";
    public static final String MODULE_VERSION_VALUE = "";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_VALUE = "true";

    @Override
    public String getLanguage() {
        return "perl";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(PerlClientCodegen.MODULE_NAME, MODULE_NAME_VALUE)
                .put(PerlClientCodegen.MODULE_VERSION, MODULE_VERSION_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, "true")
                .put("hideGenerationTimestamp", "true")
                .put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, PREPEND_FORM_OR_BODY_PARAMETERS_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
