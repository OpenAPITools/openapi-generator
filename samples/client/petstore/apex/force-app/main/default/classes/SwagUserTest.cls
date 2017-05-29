@isTest
private class SwagUserTest {
    @isTest
    private static void equalsSameInstance() {
        SwagUser user1 = SwagUser.getExample();
        SwagUser user2 = user1;
        SwagUser user3 = new SwagUser();
        SwagUser user4 = user3;

        System.assert(user1.equals(user2));
        System.assert(user2.equals(user1));
        System.assert(user1.equals(user1));
        System.assert(user3.equals(user4));
        System.assert(user4.equals(user3));
        System.assert(user3.equals(user3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        SwagUser user1 = SwagUser.getExample();
        SwagUser user2 = SwagUser.getExample();
        SwagUser user3 = new SwagUser();
        SwagUser user4 = new SwagUser();

        System.assert(user1.equals(user2));
        System.assert(user2.equals(user1));
        System.assert(user3.equals(user4));
        System.assert(user4.equals(user3));
    }

    @isTest
    private static void notEqualsUnlikeInstance() {
        SwagUser user1 = SwagUser.getExample();
        SwagUser user2 = new SwagUser();

        System.assertEquals(false, user1.equals(user2));
        System.assertEquals(false, user2.equals(user1));
    }

    @isTest
    private static void notEqualsDifferentType() {
        SwagUser user1 = SwagUser.getExample();
        SwagUser user2 = new SwagUser();

        System.assertEquals(false, user1.equals('foo'));
        System.assertEquals(false, user2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        SwagUser user1 = SwagUser.getExample();
        SwagUser user2 = new SwagUser();
        SwagUser user3;

        System.assertEquals(false, user1.equals(user3));
        System.assertEquals(false, user2.equals(user3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        SwagUser user1 = SwagUser.getExample();
        SwagUser user2 = new SwagUser();

        System.assertEquals(user1.hashCode(), user1.hashCode());
        System.assertEquals(user2.hashCode(), user2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        SwagUser user1 = SwagUser.getExample();
        SwagUser user2 = SwagUser.getExample();
        SwagUser user3 = new SwagUser();
        SwagUser user4 = new SwagUser();

        System.assert(user1.equals(user2));
        System.assert(user3.equals(user4));
        System.assertEquals(user1.hashCode(), user2.hashCode());
        System.assertEquals(user3.hashCode(), user4.hashCode());
    }
}
