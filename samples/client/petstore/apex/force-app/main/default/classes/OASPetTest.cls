@isTest
private class OASPetTest {
    @isTest
    private static void equalsSameInstance() {
        OASPet pet1 = OASPet.getExample();
        OASPet pet2 = pet1;
        OASPet pet3 = new OASPet();
        OASPet pet4 = pet3;

        System.assert(pet1.equals(pet2));
        System.assert(pet2.equals(pet1));
        System.assert(pet1.equals(pet1));
        System.assert(pet3.equals(pet4));
        System.assert(pet4.equals(pet3));
        System.assert(pet3.equals(pet3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        OASPet pet1 = OASPet.getExample();
        OASPet pet2 = OASPet.getExample();
        OASPet pet3 = new OASPet();
        OASPet pet4 = new OASPet();

        System.assert(pet1.equals(pet2));
        System.assert(pet2.equals(pet1));
        System.assert(pet3.equals(pet4));
        System.assert(pet4.equals(pet3));
    }

    @isTest
    private static void notEqualsDifferentType() {
        OASPet pet1 = OASPet.getExample();
        OASPet pet2 = new OASPet();

        System.assertEquals(false, pet1.equals('foo'));
        System.assertEquals(false, pet2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        OASPet pet1 = OASPet.getExample();
        OASPet pet2 = new OASPet();
        OASPet pet3;

        System.assertEquals(false, pet1.equals(pet3));
        System.assertEquals(false, pet2.equals(pet3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        OASPet pet1 = OASPet.getExample();
        OASPet pet2 = new OASPet();

        System.assertEquals(pet1.hashCode(), pet1.hashCode());
        System.assertEquals(pet2.hashCode(), pet2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        OASPet pet1 = OASPet.getExample();
        OASPet pet2 = OASPet.getExample();
        OASPet pet3 = new OASPet();
        OASPet pet4 = new OASPet();

        System.assert(pet1.equals(pet2));
        System.assert(pet3.equals(pet4));
        System.assertEquals(pet1.hashCode(), pet2.hashCode());
        System.assertEquals(pet3.hashCode(), pet4.hashCode());
    }

    @isTest
    private static void defaultValuesPopulated() {
        OASPet pet = new OASPet();
        System.assertEquals(new List<String>(), pet.photoUrls);
        System.assertEquals(new List<OASTag>(), pet.tags);
        System.assertEquals(null, pet.id);
        System.assertEquals(null, pet.category);
        System.assertEquals(null, pet.name);
        System.assertEquals(null, pet.status);
    }
}
