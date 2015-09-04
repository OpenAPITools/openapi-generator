/*
 *  Copyright 2015 SmartBear Software
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package io.swagger.test.integration;

import io.swagger.client.ApiClient;
import io.swagger.client.Pair;
import io.swagger.client.TypeRef;
import io.swagger.models.Operation;
import io.swagger.models.Response;
import io.swagger.models.Swagger;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.util.Json;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class ResourceListingTestIT {
    ApiClient client = new ApiClient();
    Swagger swagger = null;
    
    @BeforeClass
    public void setup() throws Exception {
      TypeRef<String> ref = new TypeRef<String>(){};
      List<Pair> queryParams = new ArrayList<Pair>();
      Map<String, String> headerParams = new HashMap<String, String>();
      Map<String, Object> formParams = new HashMap<String, Object>();

      String str = client.invokeAPI("/swagger.json", "GET", queryParams, null, headerParams, formParams, "application/json", "", new String[0], ref);
      swagger = Json.mapper().readValue(str, Swagger.class);
      assertNotNull(swagger);      
    }

    @Test
    public void verifyFileInput() throws Exception {
        Operation op = swagger.getPath("/pet/{petId}/uploadImage").getPost();
        List<Parameter> parameters = op.getParameters();
        Parameter petId = parameters.get(0);
        assertEquals(petId.getName(), "petId");
        assertEquals(petId.getIn(), "path");

        Parameter additionalMetadata = parameters.get(1);
        assertEquals(additionalMetadata.getName(), "additionalMetadata");
        assertEquals(additionalMetadata.getIn(), "formData");
        
        Parameter file = parameters.get(2);
        assertEquals(file.getName(), "file");
        assertEquals(file.getIn(), "formData");
    }

    @Test
    public void verifyMapResponse() throws Exception {
        Operation op = swagger.getPath("/store/inventory").getGet();
        Response response = op.getResponses().get("200");
        
        Property property = response.getSchema();
        assertTrue(property instanceof MapProperty);
        
        MapProperty mp = (MapProperty) property;
        assertTrue(mp.getAdditionalProperties() instanceof IntegerProperty); 
    }
}