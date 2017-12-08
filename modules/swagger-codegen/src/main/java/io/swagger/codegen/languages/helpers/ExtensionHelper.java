package io.swagger.codegen.languages.helpers;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import com.github.jknack.handlebars.Options.Buffer;
import io.swagger.codegen.VendorExtendable;

import java.io.IOException;
import java.util.Map;

public abstract class ExtensionHelper implements Helper {

    public abstract String getPreffix();

    @Override
    public Object apply(Object object, Options options) throws IOException {
        final Buffer buffer = options.buffer();

        if (object == null) {
            buffer.append(options.inverse());
            return buffer;
        }
        final String param = options.param(0);
        String extension = getPreffix() + param;

        final boolean extensionValue;

        if (object instanceof VendorExtendable) {
            extensionValue = getBooleanValue((VendorExtendable)object, extension);
        } else if (object instanceof Map) {
            extensionValue = getBooleanValue((Map)object, extension);
        } else {
            throw new RuntimeException(String.format("%s is not a valid class for this operation.", object.getClass().getName()));
        }

        if (!extensionValue) {
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
        return getBooleanValue(vendorExtendable.getVendorExtensions(), extensionKey);
    }

    public static boolean getBooleanValue(Map<String, Object> vendorExtensions, String extensionKey) {
        if (vendorExtensions == null ||vendorExtensions.isEmpty()) {
            return false;
        }
        if (vendorExtensions.get(extensionKey) == null) {
            return false;
        }
        return Boolean.parseBoolean(vendorExtensions.get(extensionKey).toString());
    }
}
