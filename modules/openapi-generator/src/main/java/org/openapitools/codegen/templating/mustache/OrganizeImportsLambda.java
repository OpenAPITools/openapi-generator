package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class OrganizeImportsLambda implements Mustache.Lambda {

    private final boolean trimWildcard;

    public OrganizeImportsLambda(boolean trimWildcard) {
        this.trimWildcard = trimWildcard;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String[] lines = fragment.execute().split("\n");
        Set<String> uniqueImports = new TreeSet<>();
        List<String> wildcardImports = new ArrayList<>();
        for (String line : lines) {
            if (line.startsWith("import ")) {
                uniqueImports.add(line.trim());
                int index = line.indexOf("*");
                if (index >= 0) {
                    wildcardImports.add(line.substring(0, index));
                }
            }
        }
        for (String uniqueImport : uniqueImports) {
            boolean skip = false;
            if (trimWildcard && uniqueImport.indexOf('*') < 0) {
                for (String wildcardImport : wildcardImports) {
                    if (uniqueImport.startsWith(wildcardImport)) {
                        int index = uniqueImport.indexOf('.', wildcardImport.length());
                        if (index < 0) {
                            // skip the import if it is a direct match with the wildcard import
                            skip = true;
                            break;
                        }
                    }
                }
            }
            if (!skip) {
                writer.write(uniqueImport + "\n");
            }
        }
    }
}
