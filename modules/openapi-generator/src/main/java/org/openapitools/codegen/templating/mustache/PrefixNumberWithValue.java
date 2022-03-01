package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

public class PrefixNumberWithValue implements Mustache.Lambda {
    private String value;

    /**
     * Constructs a new instance of {@link PrefixNumberWithValue}, with an indent count of 4 spaces
     */
    public PrefixNumberWithValue(String value, boolean isNumber) {
        this.value = value;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();
        if (text == null || text.length() == 0) {
            return;
        }

        String[] lines = text.split(System.lineSeparator());
        String line = "";
        for (String s : lines) {
            line = s;
            try {
                Integer.parseInt(line);
                if (line.equals("-1")) {
                    line = "OUTDATEDSDKVERSION";
                } else {
                    line = value + line;
                }
            } catch (Exception e) {
                // NOOP
            }
        }
        writer.write(line);
    }
}
