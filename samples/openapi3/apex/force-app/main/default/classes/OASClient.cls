public class OASClient extends OAS.ApiClient {
    public OASClient() {
        basePath = 'http://petstore.swagger.io/v2';
        calloutName = 'OpenAPI_Petstore';
        authentications.put('api_key', new OAS.ApiKeyHeaderAuth('api_key'));
    }
}
