/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import java.util.*;
import io.swagger.v3.oas.models.media.Encoding;

public class CodegenEncoding {
    public String contentType;
    public final List<CodegenProperty> headerParams= new ArrayList<CodegenProperty>();
    public String style;
    public Boolean explode;
    public Boolean allowReserved;
    public Map<String, Object> extensions =  new HashMap<>();

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenEncoding that = (CodegenEncoding) o;
        return Objects.equals(that.contentType, contentType) &&
            Objects.equals(that.headerParams, headerParams) &&
            Objects.equals(that.style, style) &&
            Objects.equals(that.explode, explode) &&
            Objects.equals(that.allowReserved, allowReserved) &&
            Objects.equals(that.extensions, extensions);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
            contentType,
            headerParams,
            style,
            explode,
            allowReserved,
            extensions);
    }

    @Override
    public String toString() {
    return "Encoding{" +
        "contentType='" + contentType + '\'' +
        ", headers=" + headerParams +
        ", style='" + style + '\'' +
        ", explode=" + explode +
        ", allowReserved=" + allowReserved +
        ", extensions=" + extensions +
        '}';
    }
}
