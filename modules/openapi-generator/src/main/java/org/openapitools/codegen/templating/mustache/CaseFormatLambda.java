package org.openapitools.codegen.templating.mustache;

import com.google.common.base.CaseFormat;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.openapitools.codegen.CodegenConfig;

import java.io.IOException;
import java.io.Writer;

/**
 * Converts text from CaseFormat to another CaseFormat
 *
 * Register:
 * <pre>
 * additionalProperties.put("convert", new CaseFormatLambda(LOWER_CAMEL, UPPER_UNDERSCORE));
 * </pre>
 *
 * Use:
 * <pre>
 * {{#convert}}{{name}}{{/convert}}
 * </pre>
 */
public class CaseFormatLambda implements Mustache.Lambda {
    private CodegenConfig generator = null;

    private CaseFormat initialFormat;
    private CaseFormat targetFormat;

    public CaseFormatLambda(CaseFormat target, CaseFormat targetFormat) {
        this.initialFormat = target;
        this.targetFormat = targetFormat;
    }

    public CaseFormatLambda generator(final CodegenConfig generator) {
        this.generator = generator;
        return this;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = initialFormat.converterTo(targetFormat).convert(fragment.execute());
        if (generator != null && generator.reservedWords().contains(text)) {
            text = generator.escapeReservedWord(text);
        }
        writer.write(text);
    }
}
