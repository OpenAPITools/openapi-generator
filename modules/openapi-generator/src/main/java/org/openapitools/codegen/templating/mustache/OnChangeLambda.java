package org.openapitools.codegen.templating.mustache;

import java.io.IOException;
import java.io.Writer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

/**
 * Lambda writes current fragment to the output when it is different than
 * the previous fragment.
 *
 * Register:
 * <pre>
 * additionalProperties.put("onchange", new OnChangeLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#onchange}}{{name}}{{/onchange}}
 * </pre>
 */
public class OnChangeLambda implements Mustache.Lambda {
    private static final Logger LOGGER = LoggerFactory.getLogger(OnChangeLambda.class);

    private String lastVal = null;

    @Override
    public void execute(Template.Fragment frag, Writer out) throws IOException {
        String curVal = frag.execute();
        LOGGER.debug("[lastVal={}, curVal={}]", lastVal, curVal);
        if (curVal != null && !curVal.equals(lastVal)) {
            out.write(curVal);
            lastVal = curVal;
        }
    }
}