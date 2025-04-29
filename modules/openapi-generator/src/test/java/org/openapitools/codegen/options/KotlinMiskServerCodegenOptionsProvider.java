package org.openapitools.codegen.options;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.AbstractKotlinCodegen;
import com.google.common.collect.ImmutableMap;

import java.util.Map;
import org.openapitools.codegen.languages.KotlinMiskServerCodegen;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;

public class KotlinMiskServerCodegenOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "org.openapitools.server.api";
    public static final String GROUP_ID_VALUE = "org.openapitools";
    public static final String ARTIFACT_ID_VALUE = "kotlin-misk-server";
    public static final String ARTIFACT_VERSION_VALUE = "0.0.1";
    public static final String SOURCE_FOLDER_VALUE = "src/main/kotlin";
    public static final String ENUM_PROPERTY_NAMING_VALUE = "camelCase";
    public static final String SERIALIZABLE_MODEL_VALUE = "false";
    public static final String PARCELIZE_MODELS_VALUE = "false";
    public static final String MODEL_MUTABLE_VALUE = "false";
    public static final String SORT_PARAMS_VALUE = "false";
    public static final String SORT_MODEL_PROPERTIES_VALUE = "false";
    public static final String API_SUFFIX_VALUE = "Api";
    public static final String ADDITIONAL_MODEL_TYPE_ANNOTATIONS_VALUE = "";
    public static final String USE_BEAN_VALIDATION = "false";
    public static final String MODULE_CLASS_NAME = "OpenApiModule";

    @Override
    public String getLanguage() {
        return "kotlin-misk";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<>();
        return builder
            .put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
            .put(CodegenConstants.GROUP_ID, GROUP_ID_VALUE)
            .put(CodegenConstants.ARTIFACT_ID, ARTIFACT_ID_VALUE)
            .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
            .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
            .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
            .put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG,
                SORT_MODEL_PROPERTIES_VALUE)
            .put(CodegenConstants.ENUM_PROPERTY_NAMING, ENUM_PROPERTY_NAMING_VALUE)
            .put(CodegenConstants.SERIALIZABLE_MODEL, SERIALIZABLE_MODEL_VALUE)
            .put(CodegenConstants.PARCELIZE_MODELS, PARCELIZE_MODELS_VALUE)
            .put(CodegenConstants.API_SUFFIX, API_SUFFIX_VALUE)
            .put(AbstractKotlinCodegen.MODEL_MUTABLE, MODEL_MUTABLE_VALUE)
            .put(AbstractKotlinCodegen.ADDITIONAL_MODEL_TYPE_ANNOTATIONS,
                ADDITIONAL_MODEL_TYPE_ANNOTATIONS_VALUE)
            .put(KotlinMiskServerCodegen.MODULE_CLASS_NAME, MODULE_CLASS_NAME)
            .put(BeanValidationFeatures.USE_BEANVALIDATION, USE_BEAN_VALIDATION)
            .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}