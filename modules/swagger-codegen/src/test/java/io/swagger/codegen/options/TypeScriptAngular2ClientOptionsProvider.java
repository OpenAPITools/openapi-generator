package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.TypeScriptAngular2ClientCodegen;

public class TypeScriptAngular2ClientOptionsProvider implements OptionsProvider {
    public static final String SUPPORTS_ES6_VALUE = "false";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    private static final String MODEL_PROPERTY_NAMING_VALUE = "camelCase";
    private static final String NMP_NAME = "npmName";
    private static final String NMP_VERSION = "1.1.2";
    private static final String NPM_REPOSITORY = "https://registry.npmjs.org";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String NG_VERSION = "2";


    @Override
    public String getLanguage() {
        return "typescript-angular2";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING_VALUE)
                .put(CodegenConstants.SUPPORTS_ES6, SUPPORTS_ES6_VALUE)
                .put(TypeScriptAngular2ClientCodegen.NPM_NAME, NMP_NAME)
                .put(TypeScriptAngular2ClientCodegen.NPM_VERSION, NMP_VERSION)
                .put(TypeScriptAngular2ClientCodegen.SNAPSHOT, Boolean.FALSE.toString())
                .put(TypeScriptAngular2ClientCodegen.WITH_INTERFACES, Boolean.FALSE.toString())
                .put(TypeScriptAngular2ClientCodegen.NPM_REPOSITORY, NPM_REPOSITORY)
                .put(TypeScriptAngular2ClientCodegen.NG_VERSION, NG_VERSION)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
