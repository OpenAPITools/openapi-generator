/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.codegen.api;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.wordnik.swagger.codegen.config.DataTypeMappingProvider;
import com.wordnik.swagger.codegen.exception.CodeGenerationException;
import com.wordnik.swagger.codegen.resource.Endpoint;
import com.wordnik.swagger.codegen.resource.Resource;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * User: deepakmichael
 * Date: 27/07/11
 * Time: 9:32 PM
 */
public class SwaggerResourceDocReader {

    private static String HEADER_NAME_API_VERSION = "Wordnik-Api-Version";

    private String baseUrl;
    private String apiKey;
    private String apiListResource;
    private ApiConfiguration apiConfiguration;
    private final DataTypeMappingProvider dataTypeMappingProvider;
    private final NamingPolicyProvider nameGenerator;

    public SwaggerResourceDocReader(ApiConfiguration apiConfiguration, DataTypeMappingProvider dataTypeMappingProvider, NamingPolicyProvider nameGenerator) {
        this.apiConfiguration = apiConfiguration;
        this.dataTypeMappingProvider = dataTypeMappingProvider;
        this.nameGenerator = nameGenerator;
        readApiConfig();

    }

    public void readApiConfig() {
        baseUrl = apiConfiguration.getApiUrl();
        apiKey = apiConfiguration.getApiKey();
        apiListResource = apiConfiguration.getApiListResource();
    }

    /**
     * Reads the documentation of the resources and constructs the resource object that can be used
     * for generating the driver related classes. The resource list string should be "," separated
     */
    public List<Resource> readResourceDocumentation() {

        List<Resource> resourceDocs = new ArrayList<Resource>();
        Client apiClient = Client.create();

        String resourceList = retrieveResourceList(apiClient);

        //valid for input
        if (baseUrl == null || resourceList == null ||
                baseUrl.trim().length() == 0 ||
                resourceList.trim().length() == 0) {
            throw new CodeGenerationException("Base URL or Resource list input is null");
        }


        //create list of resource URL
        String[] resources = resourceList.split(",");
        List<String> resourceURLs = new ArrayList<String>();
        for (String resource : resources) {
            resource = trimResourceName(resource);
            if (!resource.equals(trimResourceName( apiListResource ))) {
                if(resource.endsWith(".{format}")){
                    resource = resource.replace(".{format}", ".json");
                }
                resourceURLs.add(baseUrl + resource);
            }
        }

        //make connection to resource and get the documentation
        for (String resourceURL : resourceURLs) {
            ClientResponse clientResponse = buildClientResponse(apiClient, resourceURL);
            String response = clientResponse.getEntity(String.class);
            try {
                ObjectMapper mapper = new ObjectMapper();
                mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
                Resource aResourceDoc = deserializeResource(response, mapper);
                resourceDocs.add(aResourceDoc);
            } catch (IOException ioe) {
                ioe.printStackTrace();
                throw new CodeGenerationException("Error in coverting resource json documentation to java object");
            }
        }
        return resourceDocs;

    }

    private ClientResponse buildClientResponse(Client apiClient, String url) {
        if(null != apiKey && apiKey.length() > 0){
            url = url + "?api_key="+ apiKey;
        }
        WebResource aResource = apiClient.resource(url);
        ClientResponse clientResponse = null;
        if(null != apiKey && apiKey.length() > 0){
            aResource.header("api_key", apiKey);
            clientResponse =  aResource.header("api_key", apiKey).get(ClientResponse.class);
        }else{
            clientResponse =  aResource.get(ClientResponse.class);
        }
        return clientResponse;
    }

    private String trimResourceName(String resource) {
        if(resource.startsWith("/")){
            resource = resource.substring(1,resource.length());
        }
        return resource;
    }

    private String retrieveResourceList(Client apiClient) {
        String resourceCsv = "";
        Resource resourceApi;
        String apiResourceUrl = null;

        apiListResource = baseUrl + "resources.json";

        if(!apiListResource.endsWith(".json")){
            apiResourceUrl = trimResourceName( apiListResource.concat(".json") );
        }else{
            apiResourceUrl = trimResourceName( apiListResource);
        }

        ClientResponse clientResponse = buildClientResponse(apiClient, apiResourceUrl);
        String response = clientResponse.getEntity(String.class);
        try {
            ObjectMapper mapper = new ObjectMapper();
            mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            resourceApi = deserializeResource(response, mapper);

            for(Endpoint api: resourceApi.getEndPoints()){
                resourceCsv += (api.getPath() + ",");
            }
        }
        catch (IOException ex) {
            ex.printStackTrace();
            throw new CodeGenerationException("Error in coverting resource listing json documentation to java object");

        }
        return resourceCsv;
    }

    /**
     * Deserializes the response and returns a Response object
     * @param response
     * @param mapper
     * @return
     * @throws IOException
     */
    private Resource deserializeResource(String response, ObjectMapper mapper) throws IOException {
        Resource resource = mapper.readValue(response, Resource.class);
        resource.generateModelsFromWrapper(nameGenerator);
        return resource;
    }

}
