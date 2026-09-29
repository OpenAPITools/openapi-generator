/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.cppboostbeast;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.languages.CppBoostBeastClientCodegen;

import java.util.Map;

abstract class CppBoostBeastNormalizerTestSupport {
    protected CppBoostBeastNormalizerTestSupport() {
    }
    /**
     * Test helper that exposes protected normalizer methods as public.
     */
    static final class TestNormalizer
            extends CppBoostBeastClientCodegen.CppBoostBeastOpenAPINormalizer {
        TestNormalizer(io.swagger.v3.oas.models.OpenAPI openAPI,
                       Map<String, String> inputRules) {
            super(openAPI, inputRules);
        }

        @Override
        public Schema processSimplifyOneOf(Schema schema) {
            return super.processSimplifyOneOf(schema);
        }

        @Override
        public Schema processSimplifyAnyOf(Schema schema) {
            return super.processSimplifyAnyOf(schema);
        }

        @Override
        public Schema processSimplifyAnyOfStringAndEnumString(Schema schema) {
            return super.processSimplifyAnyOfStringAndEnumString(schema);
        }

        @Override
        public Schema processSimplifyOneOfEnum(Schema schema) {
            return super.processSimplifyOneOfEnum(schema);
        }

        @Override
        public Schema processSimplifyAnyOfEnum(Schema schema) {
            return super.processSimplifyAnyOfEnum(schema);
        }
    }
}
