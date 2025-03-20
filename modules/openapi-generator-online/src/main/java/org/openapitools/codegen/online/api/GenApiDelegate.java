/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
     * @return A {@link ResponseEntity} listing options for the target client generator.
     * @see GenApi#clientOptions
     */
    default ResponseEntity<List<String>> clientOptions() {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType : MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    ApiUtil.setExampleResponse(request, "*/*", "\"\"");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @param fileId The unique id of the file, provided in a {@link ResponseCode} response.
     * @return A {@link ResponseEntity} of the requested {@link Resource}.
     * @see GenApi#downloadFile
     */
    default ResponseEntity<Resource> downloadFile(String fileId) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @param language       The target generator (language is a misnomer here, but kept for API consistency).
     * @param generatorInput The configuration settings to be used during client generation.
     * @return A {@link ResponseEntity} referencing the unique download id and a link to download the requested client code.
     * @see GenApi#generateClient
     */
    default ResponseEntity<ResponseCode> generateClient(String language,
                                                        GeneratorInput generatorInput) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType : MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    ApiUtil.setExampleResponse(request, "*/*", "{  \"code\" : \"d40029be-eda6-4d62-b1ef-d05e2e91a72a\",  \"link\" : \"http://localhost:80/api/gen/download/d40029be-eda6-4d62-b1ef-d05e2e91a72a\"}");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @param framework      The target generator name (framework is a slight misnomer here, as we may have a framework like Spring implemented in multiple languages).
     * @param generatorInput The configuration settings to be used during server generation.
     * @return A {@link ResponseEntity} referencing the unique download id and a link to download the requested server code.
     * @see GenApi#generateServerForLanguage
     */
    default ResponseEntity<ResponseCode> generateServerForLanguage(String framework,
                                                                   GeneratorInput generatorInput) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType : MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    ApiUtil.setExampleResponse(request, "*/*", "{  \"code\" : \"d40029be-eda6-4d62-b1ef-d05e2e91a72a\",  \"link\" : \"http://localhost:80/api/gen/download/d40029be-eda6-4d62-b1ef-d05e2e91a72a\"}");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @param language The target generator (language is a misnomer here, but kept for API consistency).
     * @return A {@link ResponseEntity} of {@link CliOption}, grouped by language (generator name).
     * @see GenApi#getClientOptions
     */
    default ResponseEntity<Map<String, CliOption>> getClientOptions(String language) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @param framework The target generator name (framework is a slight misnomer here, as we may have a framework like Spring implemented in multiple languages).
     * @return A {@link ResponseEntity} of {@link CliOption}, grouped by framework (generator name).
     * @see GenApi#getServerOptions
     */
    default ResponseEntity<Map<String, CliOption>> getServerOptions(String framework) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @return A {@link ResponseEntity} listing options for the target server generator.
     * @see GenApi#serverOptions
     */
    default ResponseEntity<List<String>> serverOptions() {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType : MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    ApiUtil.setExampleResponse(request, "*/*", "\"\"");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

}
