@isTest
private class SwagOrderTest {
    @isTest
    private static void equalsSameInstance() {
        SwagOrder order1 = SwagOrder.getExample();
        SwagOrder order2 = order1;
        SwagOrder order3 = new SwagOrder();
        SwagOrder order4 = order3;

        System.assert(order1.equals(order2));
        System.assert(order2.equals(order1));
        System.assert(order1.equals(order1));
        System.assert(order3.equals(order4));
        System.assert(order4.equals(order3));
        System.assert(order3.equals(order3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        SwagOrder order1 = SwagOrder.getExample();
        SwagOrder order2 = SwagOrder.getExample();
        SwagOrder order3 = new SwagOrder();
        SwagOrder order4 = new SwagOrder();

        System.assert(order1.equals(order2));
        System.assert(order2.equals(order1));
        System.assert(order3.equals(order4));
        System.assert(order4.equals(order3));
    }

    @isTest
    private static void notEqualsUnlikeInstance() {
        SwagOrder order1 = SwagOrder.getExample();
        SwagOrder order2 = new SwagOrder();

        System.assertEquals(false, order1.equals(order2));
        System.assertEquals(false, order2.equals(order1));
    }

    @isTest
    private static void notEqualsDifferentType() {
        SwagOrder order1 = SwagOrder.getExample();
        SwagOrder order2 = new SwagOrder();

        System.assertEquals(false, order1.equals('foo'));
        System.assertEquals(false, order2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        SwagOrder order1 = SwagOrder.getExample();
        SwagOrder order2 = new SwagOrder();
        SwagOrder order3;

        System.assertEquals(false, order1.equals(order3));
        System.assertEquals(false, order2.equals(order3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        SwagOrder order1 = SwagOrder.getExample();
        SwagOrder order2 = new SwagOrder();

        System.assertEquals(order1.hashCode(), order1.hashCode());
        System.assertEquals(order2.hashCode(), order2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        SwagOrder order1 = SwagOrder.getExample();
        SwagOrder order2 = SwagOrder.getExample();
        SwagOrder order3 = new SwagOrder();
        SwagOrder order4 = new SwagOrder();

        System.assert(order1.equals(order2));
        System.assert(order3.equals(order4));
        System.assertEquals(order1.hashCode(), order2.hashCode());
        System.assertEquals(order3.hashCode(), order4.hashCode());
    }

    @isTest
    private static void defaultValuesPopulated() {
        SwagOrder order = new SwagOrder();
        System.assertEquals(false, order.complete);
        System.assertEquals(null, order.id);
        System.assertEquals(null, order.petId);
        System.assertEquals(null, order.quantity);
        System.assertEquals(null, order.shipDate);
        System.assertEquals(null, order.status);
    }
}
