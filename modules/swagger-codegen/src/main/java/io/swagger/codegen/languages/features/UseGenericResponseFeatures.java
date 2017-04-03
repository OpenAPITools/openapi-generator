package io.swagger.codegen.languages.features;

public interface UseGenericResponseFeatures {

    // Language supports generating generic Jaxrs or native return types
    public static final String USE_GENERIC_RESPONSE = "useGenericResponse";

    public void setUseGenericResponse(boolean useGenericResponse);
}
