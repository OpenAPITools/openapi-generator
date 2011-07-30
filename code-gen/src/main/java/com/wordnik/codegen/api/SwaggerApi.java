package com.wordnik.codegen.api;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.wordnik.codegen.config.ApiConfiguration;
import com.wordnik.codegen.config.DataTypeMappingProvider;
import com.wordnik.codegen.config.NamingPolicyProvider;
import com.wordnik.codegen.resource.Endpoint;
import com.wordnik.codegen.resource.Resource;
import com.wordnik.exception.CodeGenerationException;
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
public class SwaggerApi {

    private static String HEADER_NAME_API_VERSION = "Wordnik-Api-Version";

    private String baseUrl;
    private String apiKey;
    private String apiListResource;
    private ApiConfiguration apiConfiguration;
    private final DataTypeMappingProvider dataTypeMappingProvider;
    private final NamingPolicyProvider nameGenerator;

    public SwaggerApi(ApiConfiguration apiConfiguration, DataTypeMappingProvider dataTypeMappingProvider, NamingPolicyProvider nameGenerator) {
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
                if(!resource.endsWith(".json")){
                    resource = resource.concat(".json");
                }
                resourceURLs.add(baseUrl + resource);
            }
        }

        //make connection to resource and get the documentation
        for (String resourceURL : resourceURLs) {
            WebResource aResource = apiClient.resource(resourceURL);
            aResource.header("api_key", apiKey);
            ClientResponse clientResponse =  aResource.header("api_key", apiKey).get(ClientResponse.class);
            String version = clientResponse.getHeaders().get(HEADER_NAME_API_VERSION).get(0);//TODO - check if this is required
            String response = clientResponse.getEntity(String.class);
            try {
                ObjectMapper mapper = new ObjectMapper();
                mapper.getDeserializationConfig().set(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
                Resource aResourceDoc = deserializeResource(response, mapper);
                aResourceDoc.setApiVersion(version);
                resourceDocs.add(aResourceDoc);
            } catch (IOException ioe) {
                ioe.printStackTrace();
                throw new CodeGenerationException("Error in coverting resource json documentation to java object");
            }
        }
        return resourceDocs;

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
        if(apiListResource == null){
            throw new CodeGenerationException("apiListingUrl needs to be defined in api configuration object");
        }
        if(!apiListResource.endsWith(".json")){
            apiResourceUrl = trimResourceName( apiListResource.concat(".json") );
        }

        apiResourceUrl = baseUrl.concat(apiResourceUrl);

        WebResource aResource = apiClient.resource(apiResourceUrl);
        aResource.header("api_key", apiKey);
        ClientResponse clientResponse =  aResource.header("api_key", apiKey).get(ClientResponse.class);
        String response = clientResponse.getEntity(String.class);
        try {
            ObjectMapper mapper = new ObjectMapper();
            mapper.getDeserializationConfig().set(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            resourceApi = deserializeResource(response, mapper);

            for(Endpoint api: resourceApi.getEndPoints()){
                resourceCsv += (api.getPath() + ",");
            }
        }
        catch (IOException ex) {
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
