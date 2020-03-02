package org.openapitools.codegen.v2.templates;

import org.openapitools.codegen.v2.CodegenObject;
import org.openapitools.codegen.v2.CodegenObjectVisitor;
import org.openapitools.codegen.v2.outputs.CodegenOutput;
import org.openapitools.codegen.v2.outputs.CodegenOutputTags;
import org.openapitools.codegen.v2.reflection.GenericClass;

import java.util.ArrayList;
import java.util.Collection;

public class DefaultCodegenTemplateProcessor implements CodegenTemplateProcessor {

    public DefaultCodegenTemplateProcessor() {
    }

    @Override
    public void process(CodegenObject object) {
        CodegenObjectVisitor visitor = o -> {
            Collection<CodegenOutput> outputs = o.getTagOrDefault(
                    CodegenOutputTags.OUTPUTS, new GenericClass<Collection<CodegenOutput>>() {}, ArrayList::new);
            Collection<CodegenTemplate> templates = object.getTagOrDefault(
                    CodegenTemplateTags.TEMPLATES, new GenericClass<Collection<CodegenTemplate>>() {}, ArrayList::new);

            for (CodegenTemplate template : templates) {
                CodegenTemplateEngine engine = template.getEngine();
                String name = template.getGeneratedName();
                String content = engine.process(o, template);
                outputs.add(new CodegenOutput(name, content));
            }
        };

        visitor.visit(object);
    }
}
