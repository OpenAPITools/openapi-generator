package io.swagger.codegen.options;

import io.swagger.codegen.languages.RubyClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class RubyClientOptionsProvider implements OptionsProvider {
    public static final String GEM_NAME_VALUE = "swagger_client_ruby";
    public static final String MODULE_NAME_VALUE = "SwaggerClientRuby";
    public static final String GEM_VERSION_VALUE = "1.0.0-SNAPSHOT";

    @Override
    public String getLanguage() {
        return "ruby";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(RubyClientCodegen.GEM_NAME, GEM_NAME_VALUE)
                .put(RubyClientCodegen.MODULE_NAME, MODULE_NAME_VALUE)
                .put(RubyClientCodegen.GEM_VERSION, GEM_VERSION_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
