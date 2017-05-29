@isTest
private class SwagTagTest {
    @isTest
    private static void equalsSameInstance() {
        SwagTag tag1 = SwagTag.getExample();
        SwagTag tag2 = tag1;
        SwagTag tag3 = new SwagTag();
        SwagTag tag4 = tag3;

        System.assert(tag1.equals(tag2));
        System.assert(tag2.equals(tag1));
        System.assert(tag1.equals(tag1));
        System.assert(tag3.equals(tag4));
        System.assert(tag4.equals(tag3));
        System.assert(tag3.equals(tag3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        SwagTag tag1 = SwagTag.getExample();
        SwagTag tag2 = SwagTag.getExample();
        SwagTag tag3 = new SwagTag();
        SwagTag tag4 = new SwagTag();

        System.assert(tag1.equals(tag2));
        System.assert(tag2.equals(tag1));
        System.assert(tag3.equals(tag4));
        System.assert(tag4.equals(tag3));
    }

    @isTest
    private static void notEqualsUnlikeInstance() {
        SwagTag tag1 = SwagTag.getExample();
        SwagTag tag2 = new SwagTag();

        System.assertEquals(false, tag1.equals(tag2));
        System.assertEquals(false, tag2.equals(tag1));
    }

    @isTest
    private static void notEqualsDifferentType() {
        SwagTag tag1 = SwagTag.getExample();
        SwagTag tag2 = new SwagTag();

        System.assertEquals(false, tag1.equals('foo'));
        System.assertEquals(false, tag2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        SwagTag tag1 = SwagTag.getExample();
        SwagTag tag2 = new SwagTag();
        SwagTag tag3;

        System.assertEquals(false, tag1.equals(tag3));
        System.assertEquals(false, tag2.equals(tag3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        SwagTag tag1 = SwagTag.getExample();
        SwagTag tag2 = new SwagTag();

        System.assertEquals(tag1.hashCode(), tag1.hashCode());
        System.assertEquals(tag2.hashCode(), tag2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        SwagTag tag1 = SwagTag.getExample();
        SwagTag tag2 = SwagTag.getExample();
        SwagTag tag3 = new SwagTag();
        SwagTag tag4 = new SwagTag();

        System.assert(tag1.equals(tag2));
        System.assert(tag3.equals(tag4));
        System.assertEquals(tag1.hashCode(), tag2.hashCode());
        System.assertEquals(tag3.hashCode(), tag4.hashCode());
    }
}
