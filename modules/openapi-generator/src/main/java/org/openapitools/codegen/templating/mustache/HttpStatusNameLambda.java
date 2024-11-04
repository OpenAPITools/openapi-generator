package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

import static org.openapitools.codegen.templating.mustache.HttpStatusCodesMap.STATUS_CODES;
import static org.openapitools.codegen.utils.StringUtils.camelize;

/**
* Returns the Go http.Status enumeration for the given status code.
**/
public class HttpStatusNameLambda implements Mustache.Lambda {

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        final String httpCode = fragment.execute();
        writer.write(parseHttpStatusName(httpCode));
    }

    public static String parseHttpStatusName(String httpCode) {
        try {
            int statusCode = Integer.parseInt(httpCode);
            String statusText = STATUS_CODES.get(statusCode);
            if (statusText == null) {
                statusText = "Unknown";
            }
            return statusText.replaceAll("[^a-zA-Z0-9]", "");
        } catch (NumberFormatException e) {
            return httpCode;
        }
    }
}
