package io.swagger.codegen.options;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaCXFServerCodegen;

public class JavaCXFServerOptionsProvider extends JavaOptionsProvider {
    
    public static final String GENERATE_SPRING_APPLICATION = "true";
    
    public static final String USE_SWAGGER_FEATURE = "true";

    public static final String USE_SWAGGER_UI = "true";
        
    public static final String USE_WADL_FEATURE = "true";
    
    public static final String USE_MULTIPART_FEATURE = "true";
    
    public static final String USE_GZIP_FEATURE = "true";
    
    public static final String USE_GZIP_FEATURE_FOR_TESTS = "true";

    public static final String USE_LOGGING_FEATURE = "true";
    
    public static final String USE_LOGGING_FEATURE_FOR_TESTS = "true";

    public static final String USE_BEANVALIDATION_FEATURE = "true";
    
    public static final String USE_GENERIC_RESPONSE = "true";

    public static final String USE_SPRING_ANNOTATION_CONFIG = "true";

    public static final String GENERATE_SPRING_BOOT_APPLICATION = "true";
    
    public static final String GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR = "true";

    public static final String ADD_CONSUMES_PRODUCES_JSON = "true";

    public static final String IMPL_FOLDER_VALUE = "src/main/java";

    public static final String USE_ANNOTATED_BASE_PATH = "true";

    public static final String GENERATE_NON_SPRING_APPLICATION = "true";
    
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
        builder.put(JavaCXFServerCodegen.USE_SPRING_ANNOTATION_CONFIG, USE_SPRING_ANNOTATION_CONFIG);

        builder.put(JavaCXFServerCodegen.USE_SWAGGER_FEATURE, USE_SWAGGER_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_SWAGGER_UI, USE_SWAGGER_UI);

        builder.put(JavaCXFServerCodegen.USE_WADL_FEATURE, USE_WADL_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_MULTIPART_FEATURE, USE_MULTIPART_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_GZIP_FEATURE, USE_GZIP_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_GZIP_FEATURE_FOR_TESTS, USE_GZIP_FEATURE_FOR_TESTS);
        builder.put(JavaCXFServerCodegen.USE_LOGGING_FEATURE, USE_LOGGING_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_LOGGING_FEATURE_FOR_TESTS, USE_LOGGING_FEATURE_FOR_TESTS);
        builder.put(JavaCXFServerCodegen.USE_BEANVALIDATION_FEATURE, USE_BEANVALIDATION_FEATURE);
        builder.put(JavaCXFServerCodegen.USE_GENERIC_RESPONSE, USE_GENERIC_RESPONSE);
        
        builder.put(JavaCXFServerCodegen.GENERATE_SPRING_BOOT_APPLICATION, GENERATE_SPRING_BOOT_APPLICATION);
        
        builder.put(JavaCXFServerCodegen.GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR, GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR);

        builder.put(JavaCXFServerCodegen.ADD_CONSUMES_PRODUCES_JSON, ADD_CONSUMES_PRODUCES_JSON);

        builder.put(JavaCXFServerCodegen.USE_ANNOTATED_BASE_PATH, USE_ANNOTATED_BASE_PATH);

        builder.put(JavaCXFServerCodegen.GENERATE_NON_SPRING_APPLICATION, GENERATE_NON_SPRING_APPLICATION);
        builder.put("serverPort", "3456");

        return builder.build();
        
    }
}
