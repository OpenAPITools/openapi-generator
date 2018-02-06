package io.swagger.codegen.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.codegen.CodegenConfig;

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
    private CodegenConfig generator = null;

    public LowercaseLambda() {

    }

    public LowercaseLambda generator(final CodegenConfig generator) {
        this.generator = generator;
        return this;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute().toLowerCase();
        if (generator != null && generator.reservedWords().contains(text)) {
            text = generator.escapeReservedWord(text);
        }
        writer.write(text);

    }
}
