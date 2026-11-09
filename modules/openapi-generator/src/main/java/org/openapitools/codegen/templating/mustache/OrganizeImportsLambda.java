package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;
import java.util.Set;
import java.util.TreeSet;

public class OrganizeImportsLambda implements Mustache.Lambda {

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String[] lines = fragment.execute().split("\n");
        Set<String> uniqueImports = new TreeSet<>();
        for (String line : lines) {
            if (line.startsWith("import ")) {
                uniqueImports.add(line.trim());
            }
        }
        for (String uniqueImport : uniqueImports) {
            writer.write(uniqueImport + "\n");
        }
    }
}
