package io.swagger.codegen.languages.helpers;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import com.github.jknack.handlebars.Options.Buffer;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.VendorExtendable;

import java.io.IOException;
import java.util.Map;

import static io.swagger.codegen.VendorExtendable.PREFFIX_IS;

public class ExtensionHelper implements Helper<VendorExtendable> {

    public static final String NAME = "is";

    @Override
    public Object apply(VendorExtendable vendor, Options options) throws IOException {
        final Buffer buffer = options.buffer();

        if (vendor == null) {
            buffer.append(options.inverse());
            return buffer;
        }
        final String param = options.param(0);
        String extension = PREFFIX_IS + param;

        if (!getBooleanValue(vendor, extension)) {
            buffer.append(options.inverse());
        } else {
            buffer.append(options.fn());
        }
        return buffer;
    }

    public static boolean getBooleanValue(VendorExtendable vendorExtendable, String extensionKey) {
        if (vendorExtendable == null) {
            return false;
        }
        Map<String, Object> vendorExtensions = vendorExtendable.getVendorExtensions();
        if (vendorExtensions.get(extensionKey) == null) {
            return false;
        }
        return Boolean.parseBoolean(vendorExtensions.get(extensionKey).toString());
    }
}
