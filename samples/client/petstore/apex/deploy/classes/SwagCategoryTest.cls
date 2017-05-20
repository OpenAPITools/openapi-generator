@isTest
private class SwagCategoryTest {
    @isTest
    private static void equalsSameInstance() {
        SwagCategory category1 = SwagCategory.getExample();
        SwagCategory category2 = category1;
        SwagCategory category3 = new SwagCategory();
        SwagCategory category4 = category3;

        System.assert(category1.equals(category2));
        System.assert(category2.equals(category1));
        System.assert(category1.equals(category1));
        System.assert(category3.equals(category4));
        System.assert(category4.equals(category3));
        System.assert(category3.equals(category3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        SwagCategory category1 = SwagCategory.getExample();
        SwagCategory category2 = SwagCategory.getExample();
        SwagCategory category3 = new SwagCategory();
        SwagCategory category4 = new SwagCategory();

        System.assert(category1.equals(category2));
        System.assert(category2.equals(category1));
        System.assert(category3.equals(category4));
        System.assert(category4.equals(category3));
    }

    @isTest
    private static void notEqualsUnlikeInstance() {
        SwagCategory category1 = SwagCategory.getExample();
        SwagCategory category2 = new SwagCategory();

        System.assertEquals(false, category1.equals(category2));
        System.assertEquals(false, category2.equals(category1));
    }

    @isTest
    private static void notEqualsDifferentType() {
        SwagCategory category1 = SwagCategory.getExample();
        SwagCategory category2 = new SwagCategory();

        System.assertEquals(false, category1.equals('foo'));
        System.assertEquals(false, category2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        SwagCategory category1 = SwagCategory.getExample();
        SwagCategory category2 = new SwagCategory();
        SwagCategory category3;

        System.assertEquals(false, category1.equals(category3));
        System.assertEquals(false, category2.equals(category3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        SwagCategory category1 = SwagCategory.getExample();
        SwagCategory category2 = new SwagCategory();

        System.assertEquals(category1.hashCode(), category1.hashCode());
        System.assertEquals(category2.hashCode(), category2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        SwagCategory category1 = SwagCategory.getExample();
        SwagCategory category2 = SwagCategory.getExample();
        SwagCategory category3 = new SwagCategory();
        SwagCategory category4 = new SwagCategory();

        System.assert(category1.equals(category2));
        System.assert(category3.equals(category4));
        System.assertEquals(category1.hashCode(), category2.hashCode());
        System.assertEquals(category3.hashCode(), category4.hashCode());
    }
}
