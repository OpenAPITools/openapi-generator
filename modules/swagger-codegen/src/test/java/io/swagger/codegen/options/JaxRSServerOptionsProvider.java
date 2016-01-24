package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.CodegenConstants;

import java.util.Map;

public class JaxRSServerOptionsProvider extends JavaOptionsProvider {
    public static final String JODA_DATE_LIBRARY = "joda";
    public static final String IMPL_FOLDER_VALUE = "src/main/java/impl";	

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
        Map<String, String> options = super.createOptions();

        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        builder.putAll(options)
                .put(CodegenConstants.IMPL_FOLDER, IMPL_FOLDER_VALUE)
                //.put(JavaJaxRSJersey1ServerCodegen.DATE_LIBRARY, "joda") //java.lang.IllegalArgumentException: Multiple entries with same key: dateLibrary=joda and dateLibrary=joda
                ;

        return builder.build();
    }
}
