package io.swagger.codegen.languages.helpers;

import com.github.jknack.handlebars.Options;
import io.swagger.codegen.CodegenModel;

import java.io.IOException;
import java.util.Map;

public class ExtensionHelper {

    public CharSequence ext(Map<String, Object> vendorExtension, String key, Options options) throws IOException {
        //TODO
        return null;
    }

    public static boolean getBooleanValue(Map<String, Object> vendorExtensions, String extensionKey) {
        if (vendorExtensions.get(extensionKey) == null) {
            return false;
        }
        return Boolean.parseBoolean(vendorExtensions.get(extensionKey).toString());
    }
}
