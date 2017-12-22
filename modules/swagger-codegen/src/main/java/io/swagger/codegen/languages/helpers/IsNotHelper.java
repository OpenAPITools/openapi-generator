package io.swagger.codegen.languages.helpers;

import io.swagger.codegen.VendorExtendable;

public class IsNotHelper extends NoneExtensionHelper {

    public static final String NAME = "isNot";


    @Override
    public String getPreffix() {
        return VendorExtendable.PREFIX_IS;
    }
}
