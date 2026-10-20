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

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Map;

@Setter
public class GeneratorInput {
    @Getter private JsonNode spec;
    @Getter private Map<String, String> options;
    private String openAPIUrl;
    @Getter private AuthorizationValue authorizationValue;
    //FILTER=operationId:updatePet
    @Getter private List<String> openapiNormalizer;

    @ApiModelProperty(example = "https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml")
    public String getOpenAPIUrl() {
        return openAPIUrl;
    }
}
