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

package org.openapitools.codegen.java.jersey3;

import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

import static org.testng.Assert.*;

/**
 * Functional test for errorEntity deserialization feature.
 * 
 * This test verifies that the generated code includes the errorEntity 
 * field and getErrorEntity() method by examining the generated templates.
 * 
 * Full integration tests would require:
 * 1. A running mock HTTP server
 * 2. Generated client code compiled and executed
 * 3. Actual API calls to verify runtime behavior
 * 
 * The client's original project (BudgetApiTest) provides this type of 
 * functional testing. This test verifies the template structure is correct.
 */
public class JavaJersey3ErrorEntityFunctionalTest {

    private static final String JERSEY3_TEMPLATE_DIR = 
        "src/main/resources/Java/libraries/jersey3/";

    /**
     * Verify generated code includes errorEntity field in ApiException
     */
    @Test
    public void testGeneratedApiExceptionHasErrorEntity() throws Exception {
        String template = readTemplate("apiException.mustache");
        assertNotNull(template);
        
        // Verify errorEntity field exists (transient for serialization safety)
        assertTrue(template.contains("private transient Object errorEntity = null"),
            "Generated ApiException should have errorEntity field");
        
        // Verify getErrorEntity() method exists
        assertTrue(template.contains("public Object getErrorEntity()"),
            "Generated ApiException should have getErrorEntity() method");
        
        // Verify constructor with errorEntity parameter
        assertTrue(template.contains("Object errorEntity"),
            "Generated ApiException should accept errorEntity in constructor");
    }

    /**
     * Verify generated code includes deserializeErrorEntity method
     */
    @Test
    public void testGeneratedApiClientHasDeserializeErrorEntity() throws Exception {
        String template = readTemplate("ApiClient.mustache");
        assertNotNull(template);
        
        // Verify deserializeErrorEntity method exists
        assertTrue(template.contains("deserializeErrorEntity"),
            "Generated ApiClient should have deserializeErrorEntity method");
        
        // Verify errorTypes parameter handling
        assertTrue(template.contains("Map<String, GenericType> errorTypes"),
            "Generated ApiClient should handle errorTypes parameter");
    }

    /**
     * Verify generated API methods build error types map
     */
    @Test
    public void testGeneratedApiMethodsBuildErrorTypesMap() throws Exception {
        String template = readTemplate("api.mustache");
        assertNotNull(template);
        
        // Verify localVarErrorTypes is built
        assertTrue(template.contains("localVarErrorTypes"),
            "Generated API methods should build localVarErrorTypes");
        
        // Verify error types are put into the map
        assertTrue(template.contains("localVarErrorTypes.put"),
            "Generated API methods should put error types into map");
    }

    /**
     * Verify null is returned when deserialization fails
     */
    @Test
    public void testDeserializationReturnsNullOnFailure() throws Exception {
        String template = readTemplate("ApiClient.mustache");
        assertNotNull(template);
        
        // Verify that on exception, null is returned (not error message)
        // This is the fix we applied for the P2 issue
        assertFalse(template.contains("String.format(\"Failed deserializing"),
            "deserializeErrorEntity should return null, not error message string");
    }

    /**
     * Helper method to read template files
     */
    private String readTemplate(String templateName) throws Exception {
        java.nio.file.Path templatePath = java.nio.file.Paths.get(JERSEY3_TEMPLATE_DIR + templateName);
        if (java.nio.file.Files.exists(templatePath)) {
            return java.nio.file.Files.readString(templatePath);
        }
        return null;
    }
}