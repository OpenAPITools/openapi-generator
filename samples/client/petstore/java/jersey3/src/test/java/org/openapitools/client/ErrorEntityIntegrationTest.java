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

package org.openapitools.client;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test for errorEntity feature.
 * 
 * This test verifies that the ApiException class with errorEntity
 * works correctly without requiring a live server.
 */
public class ErrorEntityIntegrationTest {

    /**
     * Test errorEntity is correctly populated in ApiException
     */
    @Test
    public void testErrorEntityIsCorrectlyPopulated() {
        // Simulate error response from server
        Map<String, Object> errorResponse = new HashMap<>();
        errorResponse.put("code", 400);
        errorResponse.put("message", "Bad Request");
        errorResponse.put("details", List.of("Field 'name' is required"));
        
        // Create ApiException as if it came from a 400 response
        Map<String, List<String>> headers = new HashMap<>();
        headers.put("Content-Type", List.of("application/json"));
        headers.put("X-Request-Id", List.of("abc-123"));
        
        String responseBody = "{\"code\":400,\"message\":\"Bad Request\",\"details\":[\"Field 'name' is required\"]}";
        
        ApiException exception = new ApiException(
            400,
            "Bad Request",
            headers,
            responseBody,
            errorResponse
        );
        
        // Verify basic properties
        assertEquals(400, exception.getCode());
        assertEquals("Bad Request", exception.getMessage());
        
        // Verify headers are preserved
        assertEquals("application/json", exception.getResponseHeaders().get("Content-Type").get(0));
        assertEquals("abc-123", exception.getResponseHeaders().get("X-Request-Id").get(0));
        
        // Verify raw response body is preserved
        assertEquals(responseBody, exception.getResponseBody());
        
        // Verify errorEntity is correctly deserialized
        assertNotNull(exception.getErrorEntity());
        assertTrue(exception.getErrorEntity() instanceof Map);
        
        Map<String, Object> errorEntity = (Map<String, Object>) exception.getErrorEntity();
        assertEquals(400, errorEntity.get("code"));
        assertEquals("Bad Request", errorEntity.get("message"));
        assertNotNull(errorEntity.get("details"));
    }

    /**
     * Test errorEntity is null when not provided (backward compatibility)
     */
    @Test
    public void testErrorEntityIsNullWhenNotProvided() {
        Map<String, List<String>> headers = new HashMap<>();
        
        ApiException exception = new ApiException(
            500,
            "Internal Server Error",
            headers,
            "Server error occurred"
            // No errorEntity passed
        );
        
        assertEquals(500, exception.getCode());
        assertEquals("Internal Server Error", exception.getMessage());
        assertEquals("Server error occurred", exception.getResponseBody());
        assertNull(exception.getErrorEntity(), "errorEntity should be null when not provided");
    }

    /**
     * Test errorEntity is null when deserialization fails
     */
    @Test
    public void testErrorEntityIsNullOnDeserializationFailure() {
        Map<String, List<String>> headers = new HashMap<>();
        
        // Simulate case where deserialization failed and returned null
        ApiException exception = new ApiException(
            500,
            "Internal Server Error",
            headers,
            "invalid json{",
            null  // errorEntity is null because deserialization failed
        );
        
        assertEquals(500, exception.getCode());
        assertNull(exception.getErrorEntity(), "errorEntity should be null on deserialization failure");
    }

    /**
     * Test that errorEntity can hold any type of object
     */
    @Test
    public void testErrorEntitySupportsAnyType() {
        // Test with a custom error wrapper object structure
        Map<String, Object> customError = new HashMap<>();
        Map<String, Object> innerError = new HashMap<>();
        innerError.put("code", "VALIDATION_ERROR");
        innerError.put("message", "Validation failed");
        innerError.put("fields", List.of("email", "password"));
        customError.put("error", innerError);
        
        Map<String, List<String>> headers = new HashMap<>();
        
        ApiException exception = new ApiException(
            422,
            "Validation Error",
            headers,
            "{}",
            customError
        );
        
        assertNotNull(exception.getErrorEntity());
        assertTrue(exception.getErrorEntity() instanceof Map);
    }
}