package org.openapitools.codegen.v2.outputs;

import org.openapitools.codegen.v2.CodegenObject;
import org.openapitools.codegen.v2.CodegenObjectVisitor;
import org.openapitools.codegen.v2.reflection.GenericClass;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;

public class ConsoleCodegenOutputProcessor implements CodegenOutputProcessor {
    public void process(CodegenObject object) {
        CodegenObjectVisitor visitor = o -> {
            Collection<CodegenOutput> outputs = o.getTagOrDefault(
                    CodegenOutputTags.OUTPUTS, new GenericClass<Collection<CodegenOutput>>() {}, ArrayList::new);
            for (CodegenOutput output : outputs) {
                System.out.printf(Locale.getDefault(),
                        "%s -> \n\n%s\n\n", output.getLocation(), output.getContent());
            }
        };

        visitor.visit(object);
    }
}
