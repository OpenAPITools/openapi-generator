@isTest
private class OASApiResponseTest {
    @isTest
    private static void equalsSameInstance() {
        OASApiResponse apiResponse1 = OASApiResponse.getExample();
        OASApiResponse apiResponse2 = apiResponse1;
        OASApiResponse apiResponse3 = new OASApiResponse();
        OASApiResponse apiResponse4 = apiResponse3;

        System.assert(apiResponse1.equals(apiResponse2));
        System.assert(apiResponse2.equals(apiResponse1));
        System.assert(apiResponse1.equals(apiResponse1));
        System.assert(apiResponse3.equals(apiResponse4));
        System.assert(apiResponse4.equals(apiResponse3));
        System.assert(apiResponse3.equals(apiResponse3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        OASApiResponse apiResponse1 = OASApiResponse.getExample();
        OASApiResponse apiResponse2 = OASApiResponse.getExample();
        OASApiResponse apiResponse3 = new OASApiResponse();
        OASApiResponse apiResponse4 = new OASApiResponse();

        System.assert(apiResponse1.equals(apiResponse2));
        System.assert(apiResponse2.equals(apiResponse1));
        System.assert(apiResponse3.equals(apiResponse4));
        System.assert(apiResponse4.equals(apiResponse3));
    }

    @isTest
    private static void notEqualsDifferentType() {
        OASApiResponse apiResponse1 = OASApiResponse.getExample();
        OASApiResponse apiResponse2 = new OASApiResponse();

        System.assertEquals(false, apiResponse1.equals('foo'));
        System.assertEquals(false, apiResponse2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        OASApiResponse apiResponse1 = OASApiResponse.getExample();
        OASApiResponse apiResponse2 = new OASApiResponse();
        OASApiResponse apiResponse3;

        System.assertEquals(false, apiResponse1.equals(apiResponse3));
        System.assertEquals(false, apiResponse2.equals(apiResponse3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        OASApiResponse apiResponse1 = OASApiResponse.getExample();
        OASApiResponse apiResponse2 = new OASApiResponse();

        System.assertEquals(apiResponse1.hashCode(), apiResponse1.hashCode());
        System.assertEquals(apiResponse2.hashCode(), apiResponse2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        OASApiResponse apiResponse1 = OASApiResponse.getExample();
        OASApiResponse apiResponse2 = OASApiResponse.getExample();
        OASApiResponse apiResponse3 = new OASApiResponse();
        OASApiResponse apiResponse4 = new OASApiResponse();

        System.assert(apiResponse1.equals(apiResponse2));
        System.assert(apiResponse3.equals(apiResponse4));
        System.assertEquals(apiResponse1.hashCode(), apiResponse2.hashCode());
        System.assertEquals(apiResponse3.hashCode(), apiResponse4.hashCode());
    }

    @isTest
    private static void maintainRenamedProperties() {
        OASApiResponse apiResponse = new OASApiResponse();
        Map<String, String> propertyMappings = apiResponse.getPropertyMappings();
        System.assertEquals('r_type', propertyMappings.get('type'));
    }
}
