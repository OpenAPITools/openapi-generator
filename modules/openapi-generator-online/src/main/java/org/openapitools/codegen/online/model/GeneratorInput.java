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

package org.openapitools.codegen.online.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Map;

@Getter
@Setter
@Schema(description = "Configuration for building the client library")
public class GeneratorInput {
    
    @Schema(description = "OpenAPI specification as JSON object", example = "{\"openapi\": \"3.0.0\", \"info\": {\"title\": \"Sample API\", \"version\": \"1.0.0\"}}")
    private JsonNode spec;
    
    @Schema(description = "Generator-specific options", example = "{\"packageName\": \"com.example.client\", \"clientPackage\": \"com.example.client\"}")
    private Map<String, String> options;
    
    @Schema(description = "URL to the OpenAPI specification", example = "https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml", required = false)
    @JsonProperty("openAPIUrl")
    private String openAPIUrl;
    
    @Schema(description = "Authorization value for accessing the OpenAPI specification")
    private AuthorizationValue authorizationValue;
    
    @Schema(description = "OpenAPI normalizer rules", example = "[\"FILTER=operationId:updatePet\"]")
    private List<String> openapiNormalizer;
}
