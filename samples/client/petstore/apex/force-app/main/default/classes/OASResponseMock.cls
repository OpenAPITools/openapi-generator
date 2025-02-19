@isTest
public class OASResponseMock implements HttpCalloutMock {
    private final HttpResponse response;
    private HttpRequest request;

    public OASResponseMock(HttpResponse response) {
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