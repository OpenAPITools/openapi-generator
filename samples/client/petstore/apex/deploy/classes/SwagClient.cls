public class SwagClient extends Swagger.ApiClient {
    public SwagClient() {
        basePath = 'http://petstore.swagger.io/v2';
        authentications.put('api_key', new Swagger.ApiKeyHeaderAuth('api_key'));
        authentications.put('petstore_auth', new Swagger.OAuth2());
    }
}
