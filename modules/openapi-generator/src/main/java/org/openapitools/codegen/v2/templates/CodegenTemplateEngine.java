package org.openapitools.codegen.v2.templates;

import org.openapitools.codegen.v2.CodegenObject;

public interface CodegenTemplateEngine {
    String process(CodegenObject object, CodegenTemplate template);
}
