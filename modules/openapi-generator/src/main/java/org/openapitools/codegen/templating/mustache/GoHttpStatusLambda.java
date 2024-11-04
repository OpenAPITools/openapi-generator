package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

import static org.openapitools.codegen.templating.mustache.HttpStatusCodesMap.STATUS_CODES;
import static org.openapitools.codegen.templating.mustache.HttpStatusNameLambda.parseHttpStatusName;
import static org.openapitools.codegen.utils.StringUtils.camelize;

/**
* Returns the Go http.Status enumeration for the given status code.
**/
public class GoHttpStatusLambda implements Mustache.Lambda {

    final static String HTTP_STATUS_PREFIX = "http.Status";

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        final String httpCode = fragment.execute();
        writer.write(HTTP_STATUS_PREFIX + camelize(parseHttpStatusName(httpCode)));
    }
}
