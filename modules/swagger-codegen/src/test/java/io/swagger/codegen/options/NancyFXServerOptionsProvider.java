package io.swagger.codegen.options;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

import static io.swagger.codegen.CodegenConstants.*;

public class NancyFXServerOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_server_nancyfx";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String SOURCE_FOLDER_VALUE = "src_nancyfx";
    public static final String ALLOW_UNICODE_IDENTIFIERS_VALUE = "false";
    public static final String PROJECT_GUID_VALUE = "{6885796E-A4C1-48EA-9766-CCD1563C90DF}";


    @Override
    public String getLanguage() {
        return "nancyfx";
    }

    @Override
    public Map<String, String> createOptions() {
        final ImmutableMap.Builder<String, String> builder = ImmutableMap.builder();
        return builder.put(PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(SORT_PARAMS_BY_REQUIRED_FLAG, "true")
                .put(OPTIONAL_PROJECT_GUID, PROJECT_GUID_VALUE)
                .put(USE_DATETIME_OFFSET, "true")
                .put(USE_COLLECTION, "false")
                .put(RETURN_ICOLLECTION, "false")
                .put(INTERFACE_PREFIX, "X")
                .put(ALLOW_UNICODE_IDENTIFIERS, ALLOW_UNICODE_IDENTIFIERS_VALUE)

                .build();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
