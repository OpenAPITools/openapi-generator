@isTest
private class OASUserApiTest {
    /**
     * Create user
     *
     * This can only be done by the logged in user.
     */
    @isTest
    private static void createUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(201);
        res.setStatus('Created');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'oaSUser' => OASUser.getExample()
        };

        OASClient client;
        OASUserApi api;
        String js = '';

        api = new OASUserApi(new OASClient());

        api.createUser(params);
    }

    /**
     * Creates list of users with given input array
     *
     * 
     */
    @isTest
    private static void createUsersWithArrayInputTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'oaSUser' => new List<OASUser>{OASUser.getExample()}
        };

        OASClient client;
        OASUserApi api;
        String js = '';

        api = new OASUserApi(new OASClient());

        api.createUsersWithArrayInput(params);
    }

    /**
     * Creates list of users with given input array
     *
     * 
     */
    @isTest
    private static void createUsersWithListInputTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'oaSUser' => new List<OASUser>{OASUser.getExample()}
        };

        OASClient client;
        OASUserApi api;
        String js = '';

        api = new OASUserApi(new OASClient());

        api.createUsersWithListInput(params);
    }

    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     */
    @isTest
    private static void deleteUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'username' => 'null'
        };

        OASClient client;
        OASUserApi api;
        String js = '';

        api = new OASUserApi(new OASClient());

        api.deleteUser(params);
    }

    /**
     * Get user by user name
     *
     * 
     */
    @isTest
    private static void getUserByNameTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'username' => 'null'
        };

        OASClient client;
        OASUserApi api;
        OASUser response;
        OASUser expectedResponse;
        String js = '';

        api = new OASUserApi(new OASClient());

        js = JSON.serialize(OASUser.getExample());
        res.setHeader('Content-Type', 'application/json');
        res.setBody(js);
        expectedResponse = OASUser.getExample();
        response = (OASUser) api.getUserByName(params);
        System.assertEquals(expectedResponse, response);

        js = JSON.serialize(OASUser.getExample());
        res.setHeader('Content-Type', 'application/xml');
        res.setBody(js);
        expectedResponse = OASUser.getExample();
        response = (OASUser) api.getUserByName(params);
        System.assertEquals(expectedResponse, response);
    }

    /**
     * Logs user into the system
     *
     * 
     */
    @isTest
    private static void loginUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'username' => 'null',
            'password' => 'null'
        };

        OASClient client;
        OASUserApi api;
        String response;
        String expectedResponse;
        String js = '';

        api = new OASUserApi(new OASClient());

        api.loginUser(params);
    }

    /**
     * Logs out current logged in user session
     *
     * 
     */
    @isTest
    private static void logoutUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        OASClient client;
        OASUserApi api;
        String js = '';

        api = new OASUserApi(new OASClient());

        api.logoutUser();
    }

    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     */
    @isTest
    private static void updateUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'username' => 'null',
            'oaSUser' => OASUser.getExample()
        };

        OASClient client;
        OASUserApi api;
        String js = '';

        api = new OASUserApi(new OASClient());

        api.updateUser(params);
    }
}