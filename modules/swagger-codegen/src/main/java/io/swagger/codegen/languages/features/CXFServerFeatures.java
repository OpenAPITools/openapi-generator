package io.swagger.codegen.languages.features;

/**
 * Features supported by CXF 3 server
 *
 */
public interface CXFServerFeatures extends CXFFeatures, SwaggerFeatures, SpringFeatures {
    
    public static final String USE_WADL_FEATURE = "useWadlFeature";
    
    public static final String USE_MULTIPART_FEATURE = "useMultipartFeature";
    
    public void setUseWadlFeature(boolean useWadlFeature);

    public void setUseMultipartFeature(boolean useMultipartFeature);

}
