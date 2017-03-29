package io.swagger.codegen.options;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaCXFServerCodegen;
import io.swagger.codegen.languages.JavaResteasyServerCodegen;

public class JavaResteasyServerOptionsProvider extends JavaOptionsProvider {
    
    public static final String GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR = "true";

    public static final String IMPL_FOLDER_VALUE = "src/main/java";
    
    public static final String USE_BEANVALIDATION = "true";
    
    @Override
    public boolean isServer() {
        return true;
    }

    @Override
    public String getLanguage() {
        return "jaxrs-resteasy";
    }

    @Override
    public Map<String, String> createOptions() {
        
         Map<String, String> parentOptions = super.createOptions();
        
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>()
                .putAll(parentOptions);
        
        builder.put(CodegenConstants.IMPL_FOLDER, IMPL_FOLDER_VALUE);
        builder.put("title", "Test title");
        
        builder.put(JavaCXFServerCodegen.GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR, GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR);
        builder.put(JavaResteasyServerCodegen.USE_BEANVALIDATION, USE_BEANVALIDATION);
        builder.put("serverPort", "1234");

        return builder.build();
        
    }
}
