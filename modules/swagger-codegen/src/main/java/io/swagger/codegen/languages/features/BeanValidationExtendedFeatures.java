package io.swagger.codegen.languages.features;

public interface BeanValidationExtendedFeatures {

    // Language (implementing Client/Server) supports automatic BeanValidation (1.1)
    public static final String USE_BEANVALIDATION_FEATURE = "useBeanValidationFeature";
     
    public void setUseBeanValidationFeature(boolean useBeanValidationFeature);

    
}
