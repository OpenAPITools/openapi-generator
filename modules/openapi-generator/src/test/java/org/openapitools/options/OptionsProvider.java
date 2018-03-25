package org.openapitools.codegen.options;

import java.util.Map;

public interface OptionsProvider {
    String getLanguage();
    Map<String, String> createOptions();
    boolean isServer();
}
