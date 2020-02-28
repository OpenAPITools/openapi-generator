package org.openapitools.codegen.v2.templates.mustache;

import com.samskivert.mustache.DefaultCollector;
import com.samskivert.mustache.Mustache;
import org.openapitools.codegen.v2.CodegenObject;
import org.openapitools.codegen.v2.CodegenTag;

public class MustacheCodegenCollector extends DefaultCollector {
    @Override
    public Mustache.VariableFetcher createFetcher(Object ctx, String name) {
        Mustache.VariableFetcher fetcher = super.createFetcher(ctx, name);
        if (fetcher != null) {
            return fetcher;
        } else {
            if (ctx instanceof CodegenObject) {
                CodegenObject object = (CodegenObject) ctx;
                return (x, y) -> {
                    CodegenTag tag = CodegenTag.of(name);
                    return object.getTag(tag);
                };
            }

            return null;
        }
    }
}
