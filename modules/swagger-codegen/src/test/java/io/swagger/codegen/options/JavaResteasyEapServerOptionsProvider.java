package io.swagger.codegen.options;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaCXFServerCodegen;
import io.swagger.codegen.languages.JavaResteasyEapServerCodegen;
import io.swagger.codegen.languages.JavaResteasyServerCodegen;

public class JavaResteasyEapServerOptionsProvider extends JavaOptionsProvider {
    
    public static final String GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR = "true";

    public static final String IMPL_FOLDER_VALUE = "src/main/java";
    
    public static final String USE_BEANVALIDATION = "true";
    
     public static final String USE_SWAGGER_FEATURE = "true";

    @Override
    public boolean isServer() {
        return true;
    }

    @Override
    public String getLanguage() {
          return "jaxrs-resteasy-eap";
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
          builder.put(JavaResteasyEapServerCodegen.USE_SWAGGER_FEATURE, USE_SWAGGER_FEATURE);

        return builder.build();
        
    }
}
