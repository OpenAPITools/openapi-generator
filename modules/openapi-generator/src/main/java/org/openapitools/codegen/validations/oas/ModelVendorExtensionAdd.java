package org.openapitools.codegen.validations.oas;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ModelVendorExtensionAdd {

    @Getter
    @Setter
    @JsonProperty("class")
    private Map<String, List<String>> clazz = new HashMap<>();

    @Getter
    @Setter
    private Map<String, Map<String, List<String>>> fields;

}
