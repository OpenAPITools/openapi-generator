/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.templating.mustache;

import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

public class EscapeJavaDocLambdaTest extends LambdaTest {
    private Map<String, Object> codegenContext(String defaultValue) {
        Map<String, Object> ctx = context("lambda", context("escapeJavaDoc", new EscapeJavaDocLambda()));
        Map<String, Object> param = context("defaultValue", defaultValue);
        ctx.put("allParams", java.util.Collections.singletonList(param));
        return ctx;
    }

    @Test
    public void escapesCommentTerminatorInDefaultValue() {
        Map<String, Object> ctx = codegenContext("*/*");

        test("get|*&#47;*|",
                "{{#allParams}}{{#defaultValue}}get|{{#lambda.escapeJavaDoc}}{{.}}{{/lambda.escapeJavaDoc}}|{{/defaultValue}}{{/allParams}}",
                ctx);
    }

    @Test
    public void leavesOtherDefaultValuesUnchanged() {
        Map<String, Object> ctx = codegenContext("application/json");

        test("get|application/json|",
                "{{#allParams}}{{#defaultValue}}get|{{#lambda.escapeJavaDoc}}{{.}}{{/lambda.escapeJavaDoc}}|{{/defaultValue}}{{/allParams}}",
                ctx);
    }
}
