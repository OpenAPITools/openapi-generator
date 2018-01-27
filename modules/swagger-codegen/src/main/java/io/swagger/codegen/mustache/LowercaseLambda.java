package io.swagger.codegen.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

/**
 * Converts text in a fragment to lowercase.
 *
 * Register:
 * <pre>
 * additionalProperties.put("lowercase", new LowercaseLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#lowercase}}{{httpMethod}}{{/lowercase}}
 * </pre>
 */
public class LowercaseLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();
        writer.write(text.toLowerCase());

    }
}
