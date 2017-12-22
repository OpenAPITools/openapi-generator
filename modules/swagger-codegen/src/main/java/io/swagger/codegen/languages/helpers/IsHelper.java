package io.swagger.codegen.languages.helpers;

import static io.swagger.codegen.VendorExtendable.PREFIX_IS;

public class IsHelper extends ExtensionHelper {

    public static final String NAME = "is";

    @Override
    public String getPreffix() {
        return PREFIX_IS;
    }
}
