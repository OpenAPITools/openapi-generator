package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.Writer;

/**
 * Lambda writes current fragment to the output when it is different than
 * the previous fragment.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("onchange", new OnChangeLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#onchange}}{{name}}{{/onchange}}
 * </pre>
 */
public class OnChangeLambda implements Mustache.Lambda {
    private final Logger LOGGER = LoggerFactory.getLogger(OnChangeLambda.class);

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
