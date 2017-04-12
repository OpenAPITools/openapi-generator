package io.swagger.codegen.languages.features;

/**
 * Features supported by CXF 3 server
 *
 */
public interface CXFServerFeatures
        extends CXFFeatures, SwaggerFeatures, SpringFeatures, JbossFeature, BeanValidationExtendedFeatures, SwaggerUIFeatures
{
    
    public static final String USE_WADL_FEATURE = "useWadlFeature";
    
    public static final String USE_MULTIPART_FEATURE = "useMultipartFeature";
    
    public static final String ADD_CONSUMES_PRODUCES_JSON = "addConsumesProducesJson";

    public static final String USE_ANNOTATED_BASE_PATH = "useAnnotatedBasePath";

    public static final String GENERATE_NON_SPRING_APPLICATION = "generateNonSpringApplication";

    public void setUseWadlFeature(boolean useWadlFeature);

    public void setUseMultipartFeature(boolean useMultipartFeature);

    public void setAddConsumesProducesJson(boolean addConsumesProducesJson);

    public void setUseAnnotatedBasePath(boolean useAnnotatedBasePath);

    public void setGenerateNonSpringApplication(boolean generateNonSpringApplication);

}
