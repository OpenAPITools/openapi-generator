@isTest
public class SwaggerResponseMock implements HttpCalloutMock {
    private final HttpResponse response;
    private HttpRequest request;

    public SwaggerResponseMock(HttpResponse response) {
        this.response = response;
    }

    public HttpResponse respond(HttpRequest request) {
        this.request = request;
        return response;
    }

    public HttpRequest getRequest() {
        return request;
    }
}