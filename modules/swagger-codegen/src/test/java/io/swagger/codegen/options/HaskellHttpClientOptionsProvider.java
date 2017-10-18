package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.HaskellHttpClientCodegen;

import java.util.Map;

public class HaskellHttpClientOptionsProvider implements OptionsProvider {
    public static final String MODEL_PACKAGE_VALUE = "Model";
    public static final String API_PACKAGE_VALUE = "Api";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String HIDE_GENERATION_TIMESTAMP  = "true";

    public static final String ALLOW_FROMJSON_NULLS = "true";
    public static final String ALLOW_TOJSON_NULLS = "false";
    public static final String DATETIME_FORMAT = "%Y-%m-%dT%H:%M:%S%Q%z";
    public static final String DATE_FORMAT = "%Y-%m-%d";
    public static final String MODEL_DERIVING = "";
    public static final String STRICT_FIELDS = "false";
    public static final String GENERATE_FORM_URLENCODED_INSTANCES = "true";
    public static final String GENERATE_LENSES = "true";
    public static final String GENERATE_MODEL_CONSTRUCTORS = "true";
    public static final String INLINE_CONSUMES_CONTENT_TYPES = "false";
    public static final String USE_MONAD_LOGGER = "false";

    @Override
    public String getLanguage() {
        return "haskell-http-client";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, HIDE_GENERATION_TIMESTAMP)

                .put(HaskellHttpClientCodegen.PROP_ALLOW_FROMJSON_NULLS, ALLOW_FROMJSON_NULLS)
                .put(HaskellHttpClientCodegen.PROP_ALLOW_TOJSON_NULLS, ALLOW_TOJSON_NULLS)
                .put(HaskellHttpClientCodegen.PROP_DATETIME_FORMAT, DATETIME_FORMAT)
                .put(HaskellHttpClientCodegen.PROP_DATE_FORMAT, DATE_FORMAT)
                .put(HaskellHttpClientCodegen.PROP_MODEL_DERIVING, MODEL_DERIVING)
                .put(HaskellHttpClientCodegen.PROP_GENERATE_FORM_URLENCODED_INSTANCES, GENERATE_FORM_URLENCODED_INSTANCES)
                .put(HaskellHttpClientCodegen.PROP_GENERATE_LENSES, GENERATE_LENSES)
                .put(HaskellHttpClientCodegen.PROP_GENERATE_MODEL_CONSTRUCTORS, GENERATE_MODEL_CONSTRUCTORS)
                .put(HaskellHttpClientCodegen.PROP_INLINE_CONSUMES_CONTENT_TYPES, INLINE_CONSUMES_CONTENT_TYPES)
                .put(HaskellHttpClientCodegen.PROP_STRICT_FIELDS, STRICT_FIELDS)
                .put(HaskellHttpClientCodegen.PROP_USE_MONAD_LOGGER, USE_MONAD_LOGGER)

                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
