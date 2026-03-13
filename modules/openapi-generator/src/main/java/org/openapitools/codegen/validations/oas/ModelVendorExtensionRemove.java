package org.openapitools.codegen.validations.oas;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ModelVendorExtensionRemove {

    @Getter
    @Setter
    @JsonProperty("class")
    private List<String> clazz = new ArrayList<>();

    @Getter
    @Setter
    private Map<String, List<String>> fields = new HashMap<>();
}
