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

    public static final String ALLOW_NONUNIQUE_OPERATION_IDS = "false";
    public static final String ALLOW_FROMJSON_NULLS = "true";
    public static final String ALLOW_TOJSON_NULLS = "false";
    public static final String DATETIME_FORMAT = "%Y-%m-%dT%H:%M:%S%Q%z";
    public static final String DATE_FORMAT = "%Y-%m-%d";
    public static final String MODEL_DERIVING = "";
    public static final String STRICT_FIELDS = "false";
    public static final String GENERATE_ENUMS = "true";
    public static final String GENERATE_FORM_URLENCODED_INSTANCES = "true";
    public static final String GENERATE_LENSES = "true";
    public static final String GENERATE_MODEL_CONSTRUCTORS = "true";
    public static final String INLINE_MIME_TYPES = "false";
    public static final String USE_MONAD_LOGGER = "false";

    public static final String CABAL_PACKAGE = "cabal-package";
    public static final String CABAL_VERSION = "1.0.0.0";
    public static final String BASE_MODULE = "Network.Module";
    public static final String REQUEST_TYPE = "RequestType";
    public static final String CONFIG_TYPE = "ConfigType";

    @Override
    public String getLanguage() {
        return "haskell-http-client";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                .put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, HIDE_GENERATION_TIMESTAMP)

                .put(HaskellHttpClientCodegen.PROP_ALLOW_NONUNIQUE_OPERATION_IDS, ALLOW_NONUNIQUE_OPERATION_IDS)
                .put(HaskellHttpClientCodegen.PROP_ALLOW_FROMJSON_NULLS, ALLOW_FROMJSON_NULLS)
                .put(HaskellHttpClientCodegen.PROP_ALLOW_TOJSON_NULLS, ALLOW_TOJSON_NULLS)
                .put(HaskellHttpClientCodegen.PROP_DATETIME_FORMAT, DATETIME_FORMAT)
                .put(HaskellHttpClientCodegen.PROP_DATE_FORMAT, DATE_FORMAT)
                .put(HaskellHttpClientCodegen.PROP_MODEL_DERIVING, MODEL_DERIVING)
                .put(HaskellHttpClientCodegen.PROP_GENERATE_ENUMS, GENERATE_ENUMS)
                .put(HaskellHttpClientCodegen.PROP_GENERATE_FORM_URLENCODED_INSTANCES, GENERATE_FORM_URLENCODED_INSTANCES)
                .put(HaskellHttpClientCodegen.PROP_GENERATE_LENSES, GENERATE_LENSES)
                .put(HaskellHttpClientCodegen.PROP_GENERATE_MODEL_CONSTRUCTORS, GENERATE_MODEL_CONSTRUCTORS)
                .put(HaskellHttpClientCodegen.PROP_INLINE_MIME_TYPES, INLINE_MIME_TYPES)
                .put(HaskellHttpClientCodegen.PROP_STRICT_FIELDS, STRICT_FIELDS)
                .put(HaskellHttpClientCodegen.PROP_USE_MONAD_LOGGER, USE_MONAD_LOGGER)
                .put(HaskellHttpClientCodegen.PROP_CABAL_PACKAGE, CABAL_PACKAGE)
                .put(HaskellHttpClientCodegen.PROP_CABAL_VERSION, CABAL_VERSION)
                .put(HaskellHttpClientCodegen.PROP_BASE_MODULE, BASE_MODULE)
                .put(HaskellHttpClientCodegen.PROP_REQUEST_TYPE, REQUEST_TYPE)
                .put(HaskellHttpClientCodegen.PROP_CONFIG_TYPE, CONFIG_TYPE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
