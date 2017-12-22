package io.swagger.codegen.languages.helpers;

import io.swagger.codegen.VendorExtendable;

public class HasNotHelper extends NoneExtensionHelper {

    public static final String NAME = "hasNot";

    @Override
    public String getPreffix() {
        return VendorExtendable.PREFIX_HAS;
    }
}
