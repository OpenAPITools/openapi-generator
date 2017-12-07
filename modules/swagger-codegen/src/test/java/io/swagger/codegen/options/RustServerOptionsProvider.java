package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class RustServerOptionsProvider implements OptionsProvider {
    @Override
    public String getLanguage() {
        return "rust-server";
    }

    @Override
    public Map<String, String> createOptions() {
        return ImmutableMap.of();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
