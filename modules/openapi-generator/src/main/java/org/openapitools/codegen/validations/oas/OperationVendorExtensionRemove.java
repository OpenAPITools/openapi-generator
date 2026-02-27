package org.openapitools.codegen.validations.oas;

import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class OperationVendorExtensionRemove {

    @Getter
    @Setter
    private List<String> method = new ArrayList<>();

    @Getter
    @Setter
    private Map<String, List<String>> params = new HashMap<>();
}
