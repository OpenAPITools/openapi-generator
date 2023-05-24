package org.openapitools.api;

import org.openapitools.model.ModelApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import javax.annotation.Generated;

/**
 * A delegate to be called by the {@link VersioningApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public interface VersioningApiDelegate {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

    /**
     * POST /versioning/headers
     *
     * @param versionWithDefaultValue  (required)
     * @param versionNoDefaultValue  (required)
     * @param petId ID of pet to update (required)
     * @return successful operation (status code 200)
     * @see VersioningApi#versioningHeaders
     */
    default ResponseEntity<ModelApiResponse> versioningHeaders(String versionWithDefaultValue,
        String versionNoDefaultValue,
        Long petId) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    String exampleString = "{ \"code\" : 0, \"type\" : \"type\", \"message\" : \"message\" }";
                    ApiUtil.setExampleResponse(request, "*/*", exampleString);
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /versioning/mix
     *
     * @param versionWithDefaultValueQuery  (required)
     * @param versionNoDefaultValueQuery  (required)
     * @param versionWithDefaultValueHeader  (required)
     * @param versionNoDefaultValueHeader  (required)
     * @param petId ID of pet to update (required)
     * @return successful operation (status code 200)
     * @see VersioningApi#versioningMix
     */
    default ResponseEntity<ModelApiResponse> versioningMix(String versionWithDefaultValueQuery,
        String versionNoDefaultValueQuery,
        String versionWithDefaultValueHeader,
        String versionNoDefaultValueHeader,
        Long petId) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    String exampleString = "{ \"code\" : 0, \"type\" : \"type\", \"message\" : \"message\" }";
                    ApiUtil.setExampleResponse(request, "*/*", exampleString);
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /versioning/query-params
     *
     * @param versionWithDefaultValue  (required)
     * @param versionNoDefaultValue  (required)
     * @param petId ID of pet to update (required)
     * @return successful operation (status code 200)
     * @see VersioningApi#versioningQueryParams
     */
    default ResponseEntity<ModelApiResponse> versioningQueryParams(String versionWithDefaultValue,
        String versionNoDefaultValue,
        Long petId) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    String exampleString = "{ \"code\" : 0, \"type\" : \"type\", \"message\" : \"message\" }";
                    ApiUtil.setExampleResponse(request, "*/*", exampleString);
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

}
