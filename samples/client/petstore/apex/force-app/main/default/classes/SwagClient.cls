public class SwagClient extends Swagger.ApiClient {
    public SwagClient() {
        basePath = 'http://petstore.swagger.io/v2';
        calloutName = 'OpenAPI_Petstore';
        authentications.put('api_key', new Swagger.ApiKeyHeaderAuth('api_key'));
    }
}
