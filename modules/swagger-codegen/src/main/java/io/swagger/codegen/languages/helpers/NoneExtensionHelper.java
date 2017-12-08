package io.swagger.codegen.languages.helpers;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import io.swagger.codegen.VendorExtendable;

import java.io.IOException;
import java.util.Map;

import static io.swagger.codegen.VendorExtendable.PREFIX_IS;
import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public abstract class NoneExtensionHelper implements Helper {

    public abstract String getPreffix();

    @Override
    public Object apply(Object object, Options options) throws IOException {
        final Options.Buffer buffer = options.buffer();
        if (object == null) {
            buffer.append(options.fn());
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
            buffer.append(options.fn());
        } else {
            buffer.append(options.inverse());
        }
        return buffer;
    }
}
