@isTest
private class OASUpdatePetWithFormBodyTest {
    @isTest
    private static void equalsSameInstance() {
        OASUpdatePetWithFormBody updatePetWithFormBody1 = OASUpdatePetWithFormBody.getExample();
        OASUpdatePetWithFormBody updatePetWithFormBody2 = updatePetWithFormBody1;
        OASUpdatePetWithFormBody updatePetWithFormBody3 = new OASUpdatePetWithFormBody();
        OASUpdatePetWithFormBody updatePetWithFormBody4 = updatePetWithFormBody3;

        System.assert(updatePetWithFormBody1.equals(updatePetWithFormBody2));
        System.assert(updatePetWithFormBody2.equals(updatePetWithFormBody1));
        System.assert(updatePetWithFormBody1.equals(updatePetWithFormBody1));
        System.assert(updatePetWithFormBody3.equals(updatePetWithFormBody4));
        System.assert(updatePetWithFormBody4.equals(updatePetWithFormBody3));
        System.assert(updatePetWithFormBody3.equals(updatePetWithFormBody3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        OASUpdatePetWithFormBody updatePetWithFormBody1 = OASUpdatePetWithFormBody.getExample();
        OASUpdatePetWithFormBody updatePetWithFormBody2 = OASUpdatePetWithFormBody.getExample();
        OASUpdatePetWithFormBody updatePetWithFormBody3 = new OASUpdatePetWithFormBody();
        OASUpdatePetWithFormBody updatePetWithFormBody4 = new OASUpdatePetWithFormBody();

        System.assert(updatePetWithFormBody1.equals(updatePetWithFormBody2));
        System.assert(updatePetWithFormBody2.equals(updatePetWithFormBody1));
        System.assert(updatePetWithFormBody3.equals(updatePetWithFormBody4));
        System.assert(updatePetWithFormBody4.equals(updatePetWithFormBody3));
    }

    @isTest
    private static void notEqualsDifferentType() {
        OASUpdatePetWithFormBody updatePetWithFormBody1 = OASUpdatePetWithFormBody.getExample();
        OASUpdatePetWithFormBody updatePetWithFormBody2 = new OASUpdatePetWithFormBody();

        System.assertEquals(false, updatePetWithFormBody1.equals('foo'));
        System.assertEquals(false, updatePetWithFormBody2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        OASUpdatePetWithFormBody updatePetWithFormBody1 = OASUpdatePetWithFormBody.getExample();
        OASUpdatePetWithFormBody updatePetWithFormBody2 = new OASUpdatePetWithFormBody();
        OASUpdatePetWithFormBody updatePetWithFormBody3;

        System.assertEquals(false, updatePetWithFormBody1.equals(updatePetWithFormBody3));
        System.assertEquals(false, updatePetWithFormBody2.equals(updatePetWithFormBody3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        OASUpdatePetWithFormBody updatePetWithFormBody1 = OASUpdatePetWithFormBody.getExample();
        OASUpdatePetWithFormBody updatePetWithFormBody2 = new OASUpdatePetWithFormBody();

        System.assertEquals(updatePetWithFormBody1.hashCode(), updatePetWithFormBody1.hashCode());
        System.assertEquals(updatePetWithFormBody2.hashCode(), updatePetWithFormBody2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        OASUpdatePetWithFormBody updatePetWithFormBody1 = OASUpdatePetWithFormBody.getExample();
        OASUpdatePetWithFormBody updatePetWithFormBody2 = OASUpdatePetWithFormBody.getExample();
        OASUpdatePetWithFormBody updatePetWithFormBody3 = new OASUpdatePetWithFormBody();
        OASUpdatePetWithFormBody updatePetWithFormBody4 = new OASUpdatePetWithFormBody();

        System.assert(updatePetWithFormBody1.equals(updatePetWithFormBody2));
        System.assert(updatePetWithFormBody3.equals(updatePetWithFormBody4));
        System.assertEquals(updatePetWithFormBody1.hashCode(), updatePetWithFormBody2.hashCode());
        System.assertEquals(updatePetWithFormBody3.hashCode(), updatePetWithFormBody4.hashCode());
    }
}
