package io.swagger.codegen;

import java.util.Map;

public interface VendorExtendable {

    String PREFFIX_IS = "x-is-";
    String PREFFIX_HAS = "x-has-";

    Map<String, Object> getVendorExtensions();
}
