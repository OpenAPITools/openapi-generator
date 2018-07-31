@isTest
private class OASPetApiTest {
    /**
     * Add a new pet to the store
     *
     * 
     */
    @isTest
    private static void addPetTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(201);
        res.setStatus('Created');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'oaSPet' => OASPet.getExample()
        };

        OASClient client;
        OASPetApi api;
        String js = '';

        client = new OASClient();
        api = new OASPetApi(client);
        api.addPet(params);
    }

    /**
     * Deletes a pet
     *
     * 
     */
    @isTest
    private static void deletePetTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'petId' => 2147483648L,
            'apiKey' => 'null'
        };

        OASClient client;
        OASPetApi api;
        String js = '';

        client = new OASClient();
        api = new OASPetApi(client);
        api.deletePet(params);
    }

    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     */
    @isTest
    private static void findPetsByStatusTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'status' => new List<String>{'available'}
        };

        OASClient client;
        OASPetApi api;
        List<OASPet> response;
        List<OASPet> expectedResponse;
        String js = '';

        client = new OASClient();
        api = new OASPetApi(client);

        js = JSON.serialize(new List<OASPet>{OASPet.getExample()});
        res.setHeader('Content-Type', 'application/json');
        res.setBody(js);
        expectedResponse = new List<OASPet>{OASPet.getExample()};
        response = (List<OASPet>) api.findPetsByStatus(params);
        System.assertEquals(expectedResponse, response);


        js = JSON.serialize(new List<OASPet>{OASPet.getExample()});
        res.setHeader('Content-Type', 'application/xml');
        res.setBody(js);
        expectedResponse = new List<OASPet>{OASPet.getExample()};
        response = (List<OASPet>) api.findPetsByStatus(params);
        System.assertEquals(expectedResponse, response);
    }

    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     */
    @isTest
    private static void findPetsByTagsTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'tags' => new List<String>{''}
        };

        OASClient client;
        OASPetApi api;
        List<OASPet> response;
        List<OASPet> expectedResponse;
        String js = '';

        client = new OASClient();
        api = new OASPetApi(client);

        js = JSON.serialize(new List<OASPet>{OASPet.getExample()});
        res.setHeader('Content-Type', 'application/json');
        res.setBody(js);
        expectedResponse = new List<OASPet>{OASPet.getExample()};
        response = (List<OASPet>) api.findPetsByTags(params);
        System.assertEquals(expectedResponse, response);


        js = JSON.serialize(new List<OASPet>{OASPet.getExample()});
        res.setHeader('Content-Type', 'application/xml');
        res.setBody(js);
        expectedResponse = new List<OASPet>{OASPet.getExample()};
        response = (List<OASPet>) api.findPetsByTags(params);
        System.assertEquals(expectedResponse, response);
    }

    /**
     * Find pet by ID
     *
     * Returns a single pet
     */
    @isTest
    private static void getPetByIdTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'petId' => 2147483648L
        };

        OASClient client;
        OASPetApi api;
        OASPet response;
        OASPet expectedResponse;
        String js = '';

        client = new OASClient();
        api = new OASPetApi(client);
        ((OAS.ApiKeyAuth)client.getAuthentication('api_key')).setApiKey('foo-bar-api-key');


        js = JSON.serialize(OASPet.getExample());
        res.setHeader('Content-Type', 'application/json');
        res.setBody(js);
        expectedResponse = OASPet.getExample();
        response = (OASPet) api.getPetById(params);
        System.assertEquals(expectedResponse, response);


        js = JSON.serialize(OASPet.getExample());
        res.setHeader('Content-Type', 'application/xml');
        res.setBody(js);
        expectedResponse = OASPet.getExample();
        response = (OASPet) api.getPetById(params);
        System.assertEquals(expectedResponse, response);
    }

    /**
     * Update an existing pet
     *
     * 
     */
    @isTest
    private static void updatePetTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'oaSPet' => OASPet.getExample()
        };

        OASClient client;
        OASPetApi api;
        String js = '';

        client = new OASClient();
        api = new OASPetApi(client);
        api.updatePet(params);
    }

    /**
     * Updates a pet in the store with form data
     *
     * 
     */
    @isTest
    private static void updatePetWithFormTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'petId' => 2147483648L,
            'name' => 'null',
            'status' => 'null'
        };

        OASClient client;
        OASPetApi api;
        String js = '';

        client = new OASClient();
        api = new OASPetApi(client);
        api.updatePetWithForm(params);
    }

    /**
     * uploads an image
     *
     * 
     */
    @isTest
    private static void uploadFileTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new OASResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'petId' => 2147483648L,
            'additionalMetadata' => 'null',
            'file' => Blob.valueOf('Sample text file\nContents')
        };

        OASClient client;
        OASPetApi api;
        OASApiResponse response;
        OASApiResponse expectedResponse;
        String js = '';

        client = new OASClient();
        api = new OASPetApi(client);

        js = JSON.serialize(OASApiResponse.getExample());
        res.setHeader('Content-Type', 'application/json');
        res.setBody(js);
        expectedResponse = OASApiResponse.getExample();
        response = (OASApiResponse) api.uploadFile(params);
        System.assertEquals(expectedResponse, response);
    }
}