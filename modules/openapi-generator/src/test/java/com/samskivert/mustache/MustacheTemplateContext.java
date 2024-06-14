package com.samskivert.mustache;

import org.openapitools.codegen.CodegenConstants;

import java.util.Map;

class MustacheTemplateContext extends Template.Context {
    MustacheTemplateContext(Object parent) {
        super(parent, null, 0, false, false);
    }
    public MustacheTemplateContext(Map<String, Object> additionalProperties) {
        super(additionalProperties, new MustacheTemplateContext(additionalProperties.get(CodegenConstants.CONFIG)), 0, false, false);
    }
}
