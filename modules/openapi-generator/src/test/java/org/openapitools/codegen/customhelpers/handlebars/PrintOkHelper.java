package org.openapitools.codegen.customhelpers.handlebars;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;

import java.io.IOException;

public class PrintOkHelper implements Helper<Object> {

    @Override
    public Object apply(Object context, Options options) throws IOException {
        return "OK";
    }
}
