package io.swagger.codegen.options;

import java.util.Map;

public interface OptionsProvider {
    String getLanguage();
    Map<String, String> createOptions();
    boolean isServer();
}
