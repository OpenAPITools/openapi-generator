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
import static org.testng.Assert.*;

/**
 * Tests for error entity deserialization in jersey3 client
 * 
 * These tests verify that the templates generate the correct code
 * for errorEntity feature (issue #4777)
 */
public class JavaJersey3ErrorEntityTest {

    private static final String JERSEY3_TEMPLATE_DIR = 
        "src/main/resources/Java/libraries/jersey3/";

    /**
     * Test that apiException.mustache contains errorEntity field
     */
    @Test
    public void testApiExceptionHasErrorEntityField() throws Exception {
        String template = readTemplate("apiException.mustache");
        assertNotNull(template, "apiException.mustache should exist");
        assertTrue(template.contains("errorEntity"), 
            "apiException.mustache should contain 'errorEntity' field");
    }

    /**
     * Test that apiException.mustache contains getErrorEntity method
     */
    @Test
    public void testApiExceptionHasGetErrorEntityMethod() throws Exception {
        String template = readTemplate("apiException.mustache");
        assertNotNull(template);
        assertTrue(template.contains("getErrorEntity"), 
            "apiException.mustache should contain 'getErrorEntity()' method");
    }

    /**
     * Test that ApiClient.mustache contains deserializeErrorEntity method
     */
    @Test
    public void testApiClientHasDeserializeErrorEntityMethod() throws Exception {
        String template = readTemplate("ApiClient.mustache");
        assertNotNull(template);
        assertTrue(template.contains("deserializeErrorEntity"), 
            "ApiClient.mustache should contain 'deserializeErrorEntity' method");
    }

    /**
     * Test that api.mustache contains localVarErrorTypes
     */
    @Test
    public void testApiGeneratesErrorTypesMap() throws Exception {
        String template = readTemplate("api.mustache");
        assertNotNull(template);
        assertTrue(template.contains("localVarErrorTypes"), 
            "api.mustache should contain 'localVarErrorTypes'");
    }

    /**
     * Test that invokeAPI accepts errorTypes parameter
     */
    @Test
    public void testInvokeAPIHasErrorTypesParameter() throws Exception {
        String template = readTemplate("ApiClient.mustache");
        assertNotNull(template);
        assertTrue(template.contains("errorTypes"), 
            "ApiClient.mustache should contain 'errorTypes' parameter");
    }

    /**
     * Test that template handles "default" response (uses "0" as key)
     */
    @Test
    public void testDefaultResponseHandling() throws Exception {
        String template = readTemplate("ApiClient.mustache");
        assertNotNull(template);
        assertTrue(template.contains("\"0\""), 
            "ApiClient.mustache should handle 'default' response with '0' key");
    }

    /**
     * Test error types map building pattern
     */
    @Test
    public void testErrorTypesMapBuildingPattern() throws Exception {
        String template = readTemplate("api.mustache");
        assertNotNull(template);
        // Check pattern: localVarErrorTypes.put("code", new GenericType<...>)
        assertTrue(template.contains("localVarErrorTypes.put"), 
            "api.mustache should build error types map using put");
    }

    /**
     * Test backward compatibility - null is passed for errorTypes
     */
    @Test
    public void testBackwardCompatibility() throws Exception {
        String template = readTemplate("ApiClient.mustache");
        assertNotNull(template);
        // Check that null is passed for backward compatibility
        assertTrue(template.contains(", null") || 
                   template.contains("null,") ||
                   template.contains("null/*"),
            "Backwards compatibility: null should be passed for errorTypes");
    }

    /**
     * Helper method to read template files
     */
    private String readTemplate(String templateName) throws Exception {
        java.nio.file.Path templatePath = java.nio.file.Paths.get(
            JERSEY3_TEMPLATE_DIR + templateName);
        if (!java.nio.file.Files.exists(templatePath)) {
            // Try alternate path
            templatePath = java.nio.file.Paths.get(
                "modules/openapi-generator/" + JERSEY3_TEMPLATE_DIR + templateName);
        }
        if (java.nio.file.Files.exists(templatePath)) {
            return java.nio.file.Files.readString(templatePath);
        }
        // Try classpath
        try {
            java.io.InputStream is = getClass().getClassLoader()
                .getResourceAsStream(JERSEY3_TEMPLATE_DIR + templateName);
            if (is != null) {
                return new String(is.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
            }
        } catch (Exception ignored) {}
        
        return null;
    }
}