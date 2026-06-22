package org.openapitools.codegen.validations.oas;

import lombok.Getter;
import lombok.Setter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class OperationVendorExtensionAdd {

    @Getter
    @Setter
    private Map<String, List<String>> method = new HashMap<>();

    @Getter
    @Setter
    private Map<String, Map<String, List<String>>> parameters = new HashMap<>();
}
