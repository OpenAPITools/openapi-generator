package org.openapitools.codegen.v2;

import java.util.Map;

public interface CodegenOptions {
    String getProject();
    String getVersion();
    Map<String, Object> getAdditionalOptions();
}
