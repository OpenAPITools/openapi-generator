package io.swagger.codegen;

import java.util.Map;

public interface VendorExtendable {

    String PREFIX_IS = "x-is-";
    String PREFIX_HAS = "x-has-";

    Map<String, Object> getVendorExtensions();
}
