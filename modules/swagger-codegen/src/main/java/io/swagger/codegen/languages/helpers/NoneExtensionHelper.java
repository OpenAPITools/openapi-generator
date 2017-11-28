package io.swagger.codegen.languages.helpers;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import io.swagger.codegen.VendorExtendable;

import java.io.IOException;
import java.util.Map;

import static io.swagger.codegen.VendorExtendable.PREFFIX_IS;
import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public class NoneExtensionHelper implements Helper<VendorExtendable> {

    public static final String NAME = "isNot";

    @Override
    public Object apply(VendorExtendable vendor, Options options) throws IOException {
        final Options.Buffer buffer = options.buffer();
        if (vendor == null) {
            buffer.append(options.fn());
            return buffer;
        }
        final String param = options.param(0);
        String extension = PREFFIX_IS + param;

        final Map<String, Object> vendorExtensions = vendor.getVendorExtensions();
        if (vendorExtensions == null || !getBooleanValue(vendorExtensions, extension)) {
            buffer.append(options.fn());
        } else {
            buffer.append(options.inverse());
        }
        return buffer;
    }
}
