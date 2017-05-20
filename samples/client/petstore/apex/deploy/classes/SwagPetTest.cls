@isTest
private class SwagPetTest {
    @isTest
    private static void equalsSameInstance() {
        SwagPet pet1 = SwagPet.getExample();
        SwagPet pet2 = pet1;
        SwagPet pet3 = new SwagPet();
        SwagPet pet4 = pet3;

        System.assert(pet1.equals(pet2));
        System.assert(pet2.equals(pet1));
        System.assert(pet1.equals(pet1));
        System.assert(pet3.equals(pet4));
        System.assert(pet4.equals(pet3));
        System.assert(pet3.equals(pet3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        SwagPet pet1 = SwagPet.getExample();
        SwagPet pet2 = SwagPet.getExample();
        SwagPet pet3 = new SwagPet();
        SwagPet pet4 = new SwagPet();

        System.assert(pet1.equals(pet2));
        System.assert(pet2.equals(pet1));
        System.assert(pet3.equals(pet4));
        System.assert(pet4.equals(pet3));
    }

    @isTest
    private static void notEqualsUnlikeInstance() {
        SwagPet pet1 = SwagPet.getExample();
        SwagPet pet2 = new SwagPet();

        System.assertEquals(false, pet1.equals(pet2));
        System.assertEquals(false, pet2.equals(pet1));
    }

    @isTest
    private static void notEqualsDifferentType() {
        SwagPet pet1 = SwagPet.getExample();
        SwagPet pet2 = new SwagPet();

        System.assertEquals(false, pet1.equals('foo'));
        System.assertEquals(false, pet2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        SwagPet pet1 = SwagPet.getExample();
        SwagPet pet2 = new SwagPet();
        SwagPet pet3;

        System.assertEquals(false, pet1.equals(pet3));
        System.assertEquals(false, pet2.equals(pet3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        SwagPet pet1 = SwagPet.getExample();
        SwagPet pet2 = new SwagPet();

        System.assertEquals(pet1.hashCode(), pet1.hashCode());
        System.assertEquals(pet2.hashCode(), pet2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        SwagPet pet1 = SwagPet.getExample();
        SwagPet pet2 = SwagPet.getExample();
        SwagPet pet3 = new SwagPet();
        SwagPet pet4 = new SwagPet();

        System.assert(pet1.equals(pet2));
        System.assert(pet3.equals(pet4));
        System.assertEquals(pet1.hashCode(), pet2.hashCode());
        System.assertEquals(pet3.hashCode(), pet4.hashCode());
    }

    @isTest
    private static void defaultValuesPopulated() {
        SwagPet pet = new SwagPet();
        System.assertEquals(new List<String>(), pet.photoUrls);
        System.assertEquals(new List<SwagTag>(), pet.tags);
        System.assertEquals(null, pet.id);
        System.assertEquals(null, pet.category);
        System.assertEquals(null, pet.name);
        System.assertEquals(null, pet.status);
    }
}
