package org.openapitools.codegen;

import com.samskivert.mustache.DefaultCollector;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.lang.reflect.Field;
import java.util.Map;

public class VendorExtensionCollector extends DefaultCollector {

    private final DefaultCollector defaultCollector;

    public VendorExtensionCollector() {
        this.defaultCollector = new DefaultCollector();
    }

    @Override
    public Mustache.VariableFetcher createFetcher(Object ctx, String name) {
        Class<?> cclass = ctx.getClass();

        //Mustache.VariableFetcher defaultFetcher = super.createFetcher(ctx, name);
        if (name.startsWith("vendorExtensions.")) {
            int idx = name.indexOf('.');
            String key = name.substring(idx + 1);
            return vendorExtensionFetcher(cclass, ctx, key);
        }

        Mustache.VariableFetcher fetcher = vendorExtensionFetcher(cclass, ctx, name);
        if (fetcher != null) {
            return fetcher;
        }
        // Fall back to the default behavior
        return super.createFetcher(ctx, name);
    }

    private Mustache.VariableFetcher vendorExtensionFetcher(Class<?> cclass, Object ctx, String key) {
        try {
            Field field = cclass.getField("vendorExtensions");
            Map<String, Object> vendorExtensions = (Map<String, Object>) field.get(ctx);

            Mustache.VariableFetcher fetcher
            = (c, k) -> {

                Object value = vendorExtensions.get(key);
                if (value != null) {
                    return value;
                }
                return null;
            };
            return fetcher;
        } catch (Exception e) {
        }
        return null;
    }

    protected static Mustache.VariableFetcher NOT_FOUND_FETCHER = new Mustache.VariableFetcher() {
        public Object get(Object ctx, String name) throws Exception {
            return Template.NO_FETCHER_FOUND;
        }
    };
}
