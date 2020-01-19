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

import io.swagger.annotations.ApiModelProperty;

public class ResponseCode {
    private String code;
    private String link;

    public ResponseCode() {}

    public ResponseCode(String code, String link) {
        setCode(code);
        setLink(link);
    }

    @ApiModelProperty(value = "File download code",
            example = "d40029be-eda6-4d62-b1ef-d05e2e91a72a")
    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    @ApiModelProperty(
            value = "URL for fetching the generated client",
            example = "http://localhost:8080/api/gen/download/d40029be-eda6-4d62-b1ef-d05e2e91a72a")
    public String getLink() {
        return link;
    }

    public void setLink(String link) {
        this.link = link;
    }
}
