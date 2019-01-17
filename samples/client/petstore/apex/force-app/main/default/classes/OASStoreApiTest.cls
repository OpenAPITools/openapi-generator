@isTest
private class OASStoreApiTest {
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
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'orderId' => 'null'
        };

        OASClient client;
        OASStoreApi api;
        String js = '';

        api = new OASStoreApi(new OASClient());

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
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        OASClient client;
        OASStoreApi api;
        Map<String, Integer> response;
        Map<String, Integer> expectedResponse;
        String js = '';

        client = new OASClient();
        api = new OASStoreApi(client);
        ((OAS.ApiKeyAuth)client.getAuthentication('api_key')).setApiKey('foo-bar-api-key');

        api.getInventory();
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
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'orderId' => 2147483648L
        };

        OASClient client;
        OASStoreApi api;
        OASOrder response;
        OASOrder expectedResponse;
        String js = '';

        api = new OASStoreApi(new OASClient());

        js = JSON.serialize(OASOrder.getExample());
        res.setHeader('Content-Type', 'application/json');
        res.setBody(js);
        expectedResponse = OASOrder.getExample();
        response = (OASOrder) api.getOrderById(params);
        System.assertEquals(expectedResponse, response);

        js = JSON.serialize(OASOrder.getExample());
        res.setHeader('Content-Type', 'application/xml');
        res.setBody(js);
        expectedResponse = OASOrder.getExample();
        response = (OASOrder) api.getOrderById(params);
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
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'oaSOrder' => OASOrder.getExample()
        };

        OASClient client;
        OASStoreApi api;
        OASOrder response;
        OASOrder expectedResponse;
        String js = '';

        api = new OASStoreApi(new OASClient());

        js = JSON.serialize(OASOrder.getExample());
        res.setHeader('Content-Type', 'application/json');
        res.setBody(js);
        expectedResponse = OASOrder.getExample();
        response = (OASOrder) api.placeOrder(params);
        System.assertEquals(expectedResponse, response);

        js = JSON.serialize(OASOrder.getExample());
        res.setHeader('Content-Type', 'application/xml');
        res.setBody(js);
        expectedResponse = OASOrder.getExample();
        response = (OASOrder) api.placeOrder(params);
        System.assertEquals(expectedResponse, response);
    }
}