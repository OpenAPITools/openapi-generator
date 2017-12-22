package io.swagger.codegen.languages.helpers;

import static io.swagger.codegen.VendorExtendable.PREFIX_HAS;

public class HasHelper extends ExtensionHelper {

    public static final String NAME = "has";

    @Override
    public String getPreffix() {
        return PREFIX_HAS;
    }
}
