package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaClientCodegen;

import java.util.Map;

public class JaxRSServerOptionsProvider implements OptionsProvider {
    public static final String ARTIFACT_ID_VALUE = "swagger-java-client-test";
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String INVOKER_PACKAGE_VALUE = "io.swagger.client.test";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String GROUP_ID_VALUE = "io.swagger.test";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SOURCE_FOLDER_VALUE = "src/main/java/test";
    public static final String LOCAL_PREFIX_VALUE = "tst";
    public static final String DEFAULT_LIBRARY_VALUE = "jersey2";
    public static final String SERIALIZABLE_MODEL_VALUE = "false";
    public static final String FULL_JAVA_UTIL_VALUE = "true";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    public static final String JODA_DATE_LIBRARY = "joda";
    public static final String IMPL_FOLDER_VALUE = "src/main/java/impl";
    public static final String JAXRS_DEFAULT_LIBRARY_VALUE = "jersey1";

    @Override
    public boolean isServer() {
        return true;
    }

    @Override
    public String getLanguage() {
        return "jaxrs";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        builder.put(CodegenConstants.IMPL_FOLDER, IMPL_FOLDER_VALUE)
            .put(JavaClientCodegen.DATE_LIBRARY, "joda") //java.lang.IllegalArgumentException: Multiple entries with same key: dateLibrary=joda and dateLibrary=joda
            .put(JavaClientCodegen.SUPPORT_JAVA6, "false")
            .put("title", "Test title")
            .put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
            .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
            .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
            .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
            .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
            .put(CodegenConstants.GROUP_ID, GROUP_ID_VALUE)
            .put(CodegenConstants.ARTIFACT_ID, ARTIFACT_ID_VALUE)
            .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
            .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
            .put(CodegenConstants.LOCAL_VARIABLE_PREFIX, LOCAL_PREFIX_VALUE)
            .put(CodegenConstants.SERIALIZABLE_MODEL, SERIALIZABLE_MODEL_VALUE)
            .put(JavaClientCodegen.FULL_JAVA_UTIL, FULL_JAVA_UTIL_VALUE)
            .put(CodegenConstants.LIBRARY, JAXRS_DEFAULT_LIBRARY_VALUE)
            .put(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING, "true")
            //.put(JavaClientCodegen.DATE_LIBRARY, "joda")
            .put("hideGenerationTimestamp", "true");

        return builder.build();
    }
}
