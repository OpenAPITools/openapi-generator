@isTest
private class SwagApiResponseTest {
    @isTest
    private static void equalsSameInstance() {
        SwagApiResponse apiResponse1 = SwagApiResponse.getExample();
        SwagApiResponse apiResponse2 = apiResponse1;
        SwagApiResponse apiResponse3 = new SwagApiResponse();
        SwagApiResponse apiResponse4 = apiResponse3;

        System.assert(apiResponse1.equals(apiResponse2));
        System.assert(apiResponse2.equals(apiResponse1));
        System.assert(apiResponse1.equals(apiResponse1));
        System.assert(apiResponse3.equals(apiResponse4));
        System.assert(apiResponse4.equals(apiResponse3));
        System.assert(apiResponse3.equals(apiResponse3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        SwagApiResponse apiResponse1 = SwagApiResponse.getExample();
        SwagApiResponse apiResponse2 = SwagApiResponse.getExample();
        SwagApiResponse apiResponse3 = new SwagApiResponse();
        SwagApiResponse apiResponse4 = new SwagApiResponse();

        System.assert(apiResponse1.equals(apiResponse2));
        System.assert(apiResponse2.equals(apiResponse1));
        System.assert(apiResponse3.equals(apiResponse4));
        System.assert(apiResponse4.equals(apiResponse3));
    }

    @isTest
    private static void notEqualsUnlikeInstance() {
        SwagApiResponse apiResponse1 = SwagApiResponse.getExample();
        SwagApiResponse apiResponse2 = new SwagApiResponse();

        System.assertEquals(false, apiResponse1.equals(apiResponse2));
        System.assertEquals(false, apiResponse2.equals(apiResponse1));
    }

    @isTest
    private static void notEqualsDifferentType() {
        SwagApiResponse apiResponse1 = SwagApiResponse.getExample();
        SwagApiResponse apiResponse2 = new SwagApiResponse();

        System.assertEquals(false, apiResponse1.equals('foo'));
        System.assertEquals(false, apiResponse2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        SwagApiResponse apiResponse1 = SwagApiResponse.getExample();
        SwagApiResponse apiResponse2 = new SwagApiResponse();
        SwagApiResponse apiResponse3;

        System.assertEquals(false, apiResponse1.equals(apiResponse3));
        System.assertEquals(false, apiResponse2.equals(apiResponse3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        SwagApiResponse apiResponse1 = SwagApiResponse.getExample();
        SwagApiResponse apiResponse2 = new SwagApiResponse();

        System.assertEquals(apiResponse1.hashCode(), apiResponse1.hashCode());
        System.assertEquals(apiResponse2.hashCode(), apiResponse2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        SwagApiResponse apiResponse1 = SwagApiResponse.getExample();
        SwagApiResponse apiResponse2 = SwagApiResponse.getExample();
        SwagApiResponse apiResponse3 = new SwagApiResponse();
        SwagApiResponse apiResponse4 = new SwagApiResponse();

        System.assert(apiResponse1.equals(apiResponse2));
        System.assert(apiResponse3.equals(apiResponse4));
        System.assertEquals(apiResponse1.hashCode(), apiResponse2.hashCode());
        System.assertEquals(apiResponse3.hashCode(), apiResponse4.hashCode());
    }

    @isTest
    private static void maintainRenamedProperties() {
        SwagApiResponse apiResponse = new SwagApiResponse();
        Map<String, String> propertyMappings = apiResponse.getPropertyMappings();
        System.assertEquals('r_type', propertyMappings.get('type'));
    }
}
