package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaClientCodegen;

import java.util.Map;

public class JavaOptionsProvider implements OptionsProvider {
    public static final String ARTIFACT_ID_VALUE = "swagger-java-client-test";
    public static final String MODEL_PACKAGE_VALUE = "package";
    public static final String API_PACKAGE_VALUE = "apiPackage";
    public static final String INVOKER_PACKAGE_VALUE = "io.swagger.client.test";
    public static final String LICENSE_NAME_VALUE = "Apache License, Version 2.0";
    public static final String LICENSE_URL_VALUE = "http://www.apache.org/licenses/LICENSE-2.0";
    public static final String DEVELOPER_NAME_VALUE = "Swagger";
    public static final String DEVELOPER_EMAIL_VALUE = "apiteam@swagger.io";
    public static final String DEVELOPER_ORGANIZATION_VALUE = "Swagger";
    public static final String DEVELOPER_ORGANIZATION_URL_VALUE = "http://swagger.io";
    public static final String SCM_CONNECTION_VALUE = "scm:git:git@github.com:swagger-api/swagger-codegen.git";
    public static final String SCM_DEVELOPER_CONNECTION_VALUE = "scm:git:git@github.com:swagger-api/swagger-codegen.git";
    public static final String SCM_URL_VALUE = "https://github.com/swagger-api/swagger-codegen";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String GROUP_ID_VALUE = "io.swagger.test";
    public static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String ARTIFACT_URL_VALUE = "https://github.com/swagger-api/swagger-codegen";
    public static final String ARTIFACT_DESCRIPTION_VALUE = "Swagger Java Client Test";
    public static final String SOURCE_FOLDER_VALUE = "src/main/java/test";
    public static final String LOCAL_PREFIX_VALUE = "tst";
    public static final String SERIALIZABLE_MODEL_VALUE = "false";
    public static final String FULL_JAVA_UTIL_VALUE = "true";
    public static final String WITH_XML_VALUE = "false";
    public static final String JAVA8_MODE_VALUE = "true";
    public static final String ENSURE_UNIQUE_PARAMS_VALUE = "true";
    //public static final String SUPPORT_JAVA6 = "true";
    public static final String USE_BEANVALIDATION = "false";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";


    private ImmutableMap<String, String> options;

    /**
     * Create an options provider with the default options.
     */
    public JavaOptionsProvider() {
        options = new ImmutableMap.Builder<String, String>()
                .put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, ENSURE_UNIQUE_PARAMS_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(CodegenConstants.GROUP_ID, GROUP_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_ID, ARTIFACT_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.ARTIFACT_URL, ARTIFACT_URL_VALUE)
                .put(CodegenConstants.ARTIFACT_DESCRIPTION, ARTIFACT_DESCRIPTION_VALUE)
                .put(CodegenConstants.SCM_CONNECTION, SCM_CONNECTION_VALUE)
                .put(CodegenConstants.SCM_DEVELOPER_CONNECTION, SCM_DEVELOPER_CONNECTION_VALUE)
                .put(CodegenConstants.SCM_URL, SCM_URL_VALUE)
                .put(CodegenConstants.DEVELOPER_NAME, DEVELOPER_NAME_VALUE)
                .put(CodegenConstants.DEVELOPER_EMAIL, DEVELOPER_EMAIL_VALUE)
                .put(CodegenConstants.DEVELOPER_ORGANIZATION, DEVELOPER_ORGANIZATION_VALUE)
                .put(CodegenConstants.DEVELOPER_ORGANIZATION_URL, DEVELOPER_ORGANIZATION_URL_VALUE)
                .put(CodegenConstants.LICENSE_NAME, LICENSE_NAME_VALUE)
                .put(CodegenConstants.LICENSE_URL, LICENSE_URL_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(CodegenConstants.LOCAL_VARIABLE_PREFIX, LOCAL_PREFIX_VALUE)
                .put(CodegenConstants.SERIALIZABLE_MODEL, SERIALIZABLE_MODEL_VALUE)
                .put(JavaClientCodegen.FULL_JAVA_UTIL, FULL_JAVA_UTIL_VALUE)
                .put(JavaClientCodegen.WITH_XML, WITH_XML_VALUE)
                .put(JavaClientCodegen.JAVA8_MODE, JAVA8_MODE_VALUE)
                .put(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING, "true")
                .put(JavaClientCodegen.DATE_LIBRARY, "joda")
                .put("hideGenerationTimestamp", "true")
                .put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)
                        //.put("supportJava6", "true")
                .build();
    }

    /**
     * Use the default options, but override the ones found in additionalOptions.
     */
    public JavaOptionsProvider(Map<String, String> additionalOptions) {
         options = new ImmutableMap.Builder<String, String>()
                .putAll(options)
                .putAll(additionalOptions)
                .build();
    }

    @Override
    public Map<String, String> createOptions() {
        return options;
    }

    @Override
    public boolean isServer() {
        return false;
    }

    @Override
    public String getLanguage() {
        return "java";
    }
}
