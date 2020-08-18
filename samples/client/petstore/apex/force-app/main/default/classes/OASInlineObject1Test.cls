@isTest
private class OASInlineObject1Test {
    @isTest
    private static void equalsSameInstance() {
        OASInlineObject1 inlineObject11 = OASInlineObject1.getExample();
        OASInlineObject1 inlineObject12 = inlineObject11;
        OASInlineObject1 inlineObject13 = new OASInlineObject1();
        OASInlineObject1 inlineObject14 = inlineObject13;

        System.assert(inlineObject11.equals(inlineObject12));
        System.assert(inlineObject12.equals(inlineObject11));
        System.assert(inlineObject11.equals(inlineObject11));
        System.assert(inlineObject13.equals(inlineObject14));
        System.assert(inlineObject14.equals(inlineObject13));
        System.assert(inlineObject13.equals(inlineObject13));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        OASInlineObject1 inlineObject11 = OASInlineObject1.getExample();
        OASInlineObject1 inlineObject12 = OASInlineObject1.getExample();
        OASInlineObject1 inlineObject13 = new OASInlineObject1();
        OASInlineObject1 inlineObject14 = new OASInlineObject1();

        System.assert(inlineObject11.equals(inlineObject12));
        System.assert(inlineObject12.equals(inlineObject11));
        System.assert(inlineObject13.equals(inlineObject14));
        System.assert(inlineObject14.equals(inlineObject13));
    }

    @isTest
    private static void notEqualsDifferentType() {
        OASInlineObject1 inlineObject11 = OASInlineObject1.getExample();
        OASInlineObject1 inlineObject12 = new OASInlineObject1();

        System.assertEquals(false, inlineObject11.equals('foo'));
        System.assertEquals(false, inlineObject12.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        OASInlineObject1 inlineObject11 = OASInlineObject1.getExample();
        OASInlineObject1 inlineObject12 = new OASInlineObject1();
        OASInlineObject1 inlineObject13;

        System.assertEquals(false, inlineObject11.equals(inlineObject13));
        System.assertEquals(false, inlineObject12.equals(inlineObject13));
    }

    @isTest
    private static void consistentHashCodeValue() {
        OASInlineObject1 inlineObject11 = OASInlineObject1.getExample();
        OASInlineObject1 inlineObject12 = new OASInlineObject1();

        System.assertEquals(inlineObject11.hashCode(), inlineObject11.hashCode());
        System.assertEquals(inlineObject12.hashCode(), inlineObject12.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        OASInlineObject1 inlineObject11 = OASInlineObject1.getExample();
        OASInlineObject1 inlineObject12 = OASInlineObject1.getExample();
        OASInlineObject1 inlineObject13 = new OASInlineObject1();
        OASInlineObject1 inlineObject14 = new OASInlineObject1();

        System.assert(inlineObject11.equals(inlineObject12));
        System.assert(inlineObject13.equals(inlineObject14));
        System.assertEquals(inlineObject11.hashCode(), inlineObject12.hashCode());
        System.assertEquals(inlineObject13.hashCode(), inlineObject14.hashCode());
    }
}
