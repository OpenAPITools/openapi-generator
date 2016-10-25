package io.swagger.codegen.options;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaCXFServerCodegen;

public class JavaCXFServerOptionsProvider extends JavaOptionsProvider {
    
    public static final String GENERATE_SPRING_APPLICATION = "true";
    
    public static final String USE_SWAGGER_FEATURE = "true";
        
    public static final String USE_WADL_FEATURE = "true";
    
    public static final String USE_MULTIPART_FEATURE = "true";
    
    public static final String USE_GZIP_FEATURE = "true";
    
    public static final String USE_LOGGING_FEATURE = "true";
    
    public static final String USE_BEANVALIDATION_FEATURE = "true";
    
    public static final String GENERATE_SPRING_BOOT_APPLICATION = "true";
    
    public static final String IMPL_FOLDER_VALUE = "src/main/java";
    
    @Override
    public boolean isServer() {
        return true;
    }

    @Override
    public String getLanguage() {
        return "jaxrs-cxf";
    }

    @Override
    public Map<String, String> createOptions() {
        
         Map<String, String> parentOptions = super.createOptions();
        
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>()
                .putAll(parentOptions);
        
        builder.put(CodegenConstants.IMPL_FOLDER, IMPL_FOLDER_VALUE);
        builder.put("title", "Test title");

        builder.put(JavaCXFServerCodegen.USE_BEANVALIDATION, JavaOptionsProvider.USE_BEANVALIDATION);
        builder.put(JavaCXFServerCodegen.GENERATE_SPRING_APPLICATION, GENERATE_SPRING_APPLICATION);
        builder.put(JavaCXFServerCodegen.USE_SWAGGER_FEATURE, USE_SWAGGER_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_WADL_FEATURE, USE_WADL_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_MULTIPART_FEATURE, USE_MULTIPART_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_GZIP_FEATURE, USE_GZIP_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_LOGGING_FEATURE, USE_LOGGING_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_BEANVALIDATION_FEATURE, USE_BEANVALIDATION_FEATURE);
        
        builder.put(JavaCXFServerCodegen.GENERATE_SPRING_BOOT_APPLICATION, GENERATE_SPRING_BOOT_APPLICATION);
        
        return builder.build();
        
    }
}
