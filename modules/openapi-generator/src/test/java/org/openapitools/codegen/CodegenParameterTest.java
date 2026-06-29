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

package org.openapitools.codegen;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.testng.annotations.Test;

import java.io.StringWriter;
import java.util.Collections;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotEquals;

public class CodegenParameterTest {

    @Test
    public void equalityIncludesJsonExampleNodeWithoutHashingIt() {
        final DefaultCodegen codegen = new DefaultCodegen();
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        CodegenParameter codegenParameter2 = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);

        codegen.setParameterExampleValue(codegenParameter, Collections.singletonMap("value", "first"));
        codegen.setParameterExampleValue(codegenParameter2, Collections.singletonMap("value", "second"));

        assertNotEquals(codegenParameter, codegenParameter2);
        assertEquals(codegenParameter.hashCode(), codegenParameter2.hashCode());
    }

    @Test
    public void lambdaExampleUsesLegacyTextForJsonValueNodes() throws Exception {
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegenParameter.setExample(new ObjectMapper().readTree("\"doggie\""));

        StringWriter writer = new StringWriter();
        codegenParameter.getLambdaExample().execute(null, writer);

        assertEquals(writer.toString(), "doggie");
    }

    @Test
    public void lambdaExampleStreamsJsonForObjectNodes() throws Exception {
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegenParameter.setExample(new ObjectMapper().readTree("{\"value\":\"doggie\"}"));

        StringWriter writer = new StringWriter();
        codegenParameter.getLambdaExample().execute(null, writer);

        assertEquals(writer.toString(), "{\n  \"value\" : \"doggie\"\n}");
    }
}
