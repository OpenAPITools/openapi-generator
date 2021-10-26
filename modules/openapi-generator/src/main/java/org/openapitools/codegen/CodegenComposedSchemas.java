/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen;

import java.util.*;

public class CodegenComposedSchemas {
    private List<CodegenProperty> allOf;
    private List<CodegenProperty> oneOf;
    private List<CodegenProperty> anyOf;

    public CodegenComposedSchemas(List<CodegenProperty> allOf, List<CodegenProperty> oneOf, List<CodegenProperty> anyOf) {
        this.allOf = allOf;
        this.oneOf = oneOf;
        this.anyOf = anyOf;
    }

    public List<CodegenProperty> getAllOf() {
        return allOf;
    }

    public List<CodegenProperty> getOneOf() {
        return oneOf;
    }

    public List<CodegenProperty> getAnyOf() {
        return anyOf;
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("CodegenComposedSchemas{");
        sb.append("oneOf=").append(oneOf);
        sb.append(", anyOf=").append(anyOf);
        sb.append(", allOf=").append(allOf);
        sb.append('}');
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenComposedSchemas that = (CodegenComposedSchemas) o;
        return Objects.equals(oneOf, that.oneOf) &&
                Objects.equals(anyOf, that.anyOf) &&
                Objects.equals(allOf, that.allOf);
    }

    @Override
    public int hashCode() {
        return Objects.hash(oneOf, anyOf, allOf);
    }
}