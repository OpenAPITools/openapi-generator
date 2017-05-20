@isTest
private class SwagStoreApiTest {
    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     */
    @isTest
    private static void deleteOrderTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'orderId' => 'orderId_example'
        };

        SwagClient client;
        SwagStoreApi api;

        api = new SwagStoreApi(new SwagClient());

        api.deleteOrder(params);
    }

    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     */
    @isTest
    private static void getInventoryTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        SwagClient client;
        SwagStoreApi api;
        Map<String, Integer> response;
        Map<String, Integer> expectedResponse;

        client = new SwagClient();
        api = new SwagStoreApi(client);
        ((Swagger.ApiKeyAuth) client.getAuthentication('api_key'))
            .setApiKey('foo-bar-api-key');

        res.setHeader('Content-Type', 'application/json');
        res.setBody('{\n  "key" : 0\n}');
        expectedResponse = new Map<String, Integer>{'key'=>123};
        response = (Map<String, Integer>) api.getInventory();
        System.assertEquals(expectedResponse, response);
    }

    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     */
    @isTest
    private static void getOrderByIdTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'orderId' => 2147483648L
        };

        SwagClient client;
        SwagStoreApi api;
        SwagOrder response;
        SwagOrder expectedResponse;

        api = new SwagStoreApi(new SwagClient());

        res.setHeader('Content-Type', 'application/json');
        res.setBody('{\n  "petId" : 6,\n  "quantity" : 1,\n  "id" : 0,\n  "shipDate" : "2000-01-23T04:56:07.000+00:00",\n  "complete" : false,\n  "status" : "placed"\n}');
        expectedResponse = SwagOrder.getExample();
        response = (SwagOrder) api.getOrderById(params);
        System.assertEquals(expectedResponse, response);
    }

    /**
     * Place an order for a pet
     *
     * 
     */
    @isTest
    private static void placeOrderTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'body' => SwagOrder.getExample()
        };

        SwagClient client;
        SwagStoreApi api;
        SwagOrder response;
        SwagOrder expectedResponse;

        api = new SwagStoreApi(new SwagClient());

        res.setHeader('Content-Type', 'application/json');
        res.setBody('{\n  "petId" : 6,\n  "quantity" : 1,\n  "id" : 0,\n  "shipDate" : "2000-01-23T04:56:07.000+00:00",\n  "complete" : false,\n  "status" : "placed"\n}');
        expectedResponse = SwagOrder.getExample();
        response = (SwagOrder) api.placeOrder(params);
        System.assertEquals(expectedResponse, response);
    }
}