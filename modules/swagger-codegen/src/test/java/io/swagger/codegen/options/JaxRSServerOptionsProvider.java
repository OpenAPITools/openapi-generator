package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;
import io.swagger.codegen.languages.JaxRSServerCodegen;

import java.util.Map;

public class JaxRSServerOptionsProvider extends JavaOptionsProvider {
    public static final String JODA_DATE_LIBRARY = "joda";

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
                .put(JaxRSServerCodegen.DATE_LIBRARY, "joda");

        return builder.build();
    }
}
