package io.swagger.codegen.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

/**
 * Converts text in a fragment to uppercase.
 *
 * Register:
 * <pre>
 * additionalProperties.put("uppercase", new UppercaseLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#uppercase}}{{summary}}{{/uppercase}}
 * </pre>
 */
public class UppercaseLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();
        writer.write(text.toUpperCase());
    }
}
