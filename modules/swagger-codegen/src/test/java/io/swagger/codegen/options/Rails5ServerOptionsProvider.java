package io.swagger.codegen.options;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class Rails5ServerOptionsProvider implements OptionsProvider {
    @Override
    public String getLanguage() {
        return "Rails5";
    }

    @Override
    public Map<String, String> createOptions() {
        //Rails5ServerCodegen doesn't have its own options and base options are cleared
        return ImmutableMap.of();
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
