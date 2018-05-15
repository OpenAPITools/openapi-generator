package org.openapitools.codegen.online.api;

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.online.model.GeneratorInput;
import org.openapitools.codegen.online.model.ResponseCode;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.NativeWebRequest;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * A delegate to be called by the {@link GenApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface GenApiDelegate {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

    /**
     * @see GenApi#clientOptions
     */
    default ResponseEntity<List<String>> clientOptions() {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    ApiUtil.setExampleResponse(request, "*/*", "\"\"");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see GenApi#downloadFile
     */
    default ResponseEntity<Resource> downloadFile(String  fileId) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see GenApi#generateClient
     */
    default ResponseEntity<ResponseCode> generateClient( String  language,
         GeneratorInput  generatorInput) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    ApiUtil.setExampleResponse(request, "*/*", "{  \"code\" : \"d40029be-eda6-4d62-b1ef-d05e2e91a72a\",  \"link\" : \"http://generator.swagger.io:80/api/gen/download/d40029be-eda6-4d62-b1ef-d05e2e91a72a\"}");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see GenApi#generateServerForLanguage
     */
    default ResponseEntity<ResponseCode> generateServerForLanguage( String  framework,
         GeneratorInput  generatorInput) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    ApiUtil.setExampleResponse(request, "*/*", "{  \"code\" : \"d40029be-eda6-4d62-b1ef-d05e2e91a72a\",  \"link\" : \"http://generator.swagger.io:80/api/gen/download/d40029be-eda6-4d62-b1ef-d05e2e91a72a\"}");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see GenApi#getClientOptions
     */
    default ResponseEntity<Map<String, CliOption>> getClientOptions(String  language) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see GenApi#getServerOptions
     */
    default ResponseEntity<Map<String, CliOption>> getServerOptions( String  framework) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see GenApi#serverOptions
     */
    default ResponseEntity<List<String>> serverOptions() {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    ApiUtil.setExampleResponse(request, "*/*", "\"\"");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

}
