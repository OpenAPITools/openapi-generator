@isTest
private class OASTagTest {
    @isTest
    private static void equalsSameInstance() {
        OASTag tag1 = OASTag.getExample();
        OASTag tag2 = tag1;
        OASTag tag3 = new OASTag();
        OASTag tag4 = tag3;

        System.assert(tag1.equals(tag2));
        System.assert(tag2.equals(tag1));
        System.assert(tag1.equals(tag1));
        System.assert(tag3.equals(tag4));
        System.assert(tag4.equals(tag3));
        System.assert(tag3.equals(tag3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        OASTag tag1 = OASTag.getExample();
        OASTag tag2 = OASTag.getExample();
        OASTag tag3 = new OASTag();
        OASTag tag4 = new OASTag();

        System.assert(tag1.equals(tag2));
        System.assert(tag2.equals(tag1));
        System.assert(tag3.equals(tag4));
        System.assert(tag4.equals(tag3));
    }

    @isTest
    private static void notEqualsDifferentType() {
        OASTag tag1 = OASTag.getExample();
        OASTag tag2 = new OASTag();

        System.assertEquals(false, tag1.equals('foo'));
        System.assertEquals(false, tag2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        OASTag tag1 = OASTag.getExample();
        OASTag tag2 = new OASTag();
        OASTag tag3;

        System.assertEquals(false, tag1.equals(tag3));
        System.assertEquals(false, tag2.equals(tag3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        OASTag tag1 = OASTag.getExample();
        OASTag tag2 = new OASTag();

        System.assertEquals(tag1.hashCode(), tag1.hashCode());
        System.assertEquals(tag2.hashCode(), tag2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        OASTag tag1 = OASTag.getExample();
        OASTag tag2 = OASTag.getExample();
        OASTag tag3 = new OASTag();
        OASTag tag4 = new OASTag();

        System.assert(tag1.equals(tag2));
        System.assert(tag3.equals(tag4));
        System.assertEquals(tag1.hashCode(), tag2.hashCode());
        System.assertEquals(tag3.hashCode(), tag4.hashCode());
    }
}
