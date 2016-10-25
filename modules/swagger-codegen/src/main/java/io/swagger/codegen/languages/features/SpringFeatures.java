package io.swagger.codegen.languages.features;

public interface SpringFeatures extends BeanValidationFeatures {

    public static final String GENERATE_SPRING_APPLICATION = "generateSpringApplication";
    
    public static final String GENERATE_SPRING_BOOT_APPLICATION = "generateSpringBootApplication";

    public void setGenerateSpringApplication(boolean useGenerateSpringApplication);
    
    public void setGenerateSpringBootApplication(boolean generateSpringBootApplication);

        
}
