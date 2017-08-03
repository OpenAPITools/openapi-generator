package io.swagger.codegen.options;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

import io.swagger.codegen.languages.JavaCXFClientCodegen;

public class JavaCXFClientOptionsProvider extends JavaOptionsProvider {

    public static final String USE_BEANVALIDATION = "true";

    public static final String USE_GZIP_FEATURE_FOR_TESTS = "true";

    public static final String USE_LOGGING_FEATURE_FOR_TESTS = "true";

    public static final String USE_GENERIC_RESPONSE = "true";

    
    @Override
    public boolean isServer() {
        return false;
    }

    @Override
    public String getLanguage() {
        return "jaxrs-cxf-client";
    }

    @Override
    public Map<String, String> createOptions() {
        
         Map<String, String> parentOptions = super.createOptions();
        
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>()
                .putAll(parentOptions);
        
        builder.put(JavaCXFClientCodegen.USE_BEANVALIDATION, JavaCXFClientOptionsProvider.USE_BEANVALIDATION);
        builder.put(JavaCXFClientCodegen.USE_GENERIC_RESPONSE, JavaCXFClientOptionsProvider.USE_GENERIC_RESPONSE);

        builder.put(JavaCXFClientCodegen.USE_GZIP_FEATURE_FOR_TESTS, USE_GZIP_FEATURE_FOR_TESTS);
        builder.put(JavaCXFClientCodegen.USE_LOGGING_FEATURE_FOR_TESTS, USE_LOGGING_FEATURE_FOR_TESTS);

        return builder.build();
        
    }
}
