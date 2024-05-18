package org.openapitools.codegen.customhelpers.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

public class PrintSuccessHelper implements Mustache.Lambda {

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        writer.write("SUCCESS");
    }
}
