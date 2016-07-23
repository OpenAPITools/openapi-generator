part of tests;

testPetApi() {
  var petApi = new PetApi();

  describe('Pet API ', () {
    it('adds a new pet and gets it by id', () async {
      var id = newId();

      await petApi.addPet(new Pet()..id = id);
      var pet = await petApi.getPetById(id);
      expect(pet.id).toEqual(id);
    });

    it('doesn\'t get non-existing pet by id', () {
      expect(petApi.getPetById(newId()))
          .toThrowWith(anInstanceOf: ApiException);
    });

    it('deletes existing pet by id', () async {
      var id = newId();
      await petApi.addPet(new Pet()..id = id);
      await petApi.deletePet(id, apiKey: 'special-key');
      expect(petApi.getPetById(id)).toThrowWith(anInstanceOf: ApiException);
    });

    it('updates pet with form', () async {
      var id = newId();
      await petApi.addPet(new Pet()
        ..id = id
        ..name = 'Snowy');
      await petApi.updatePetWithForm(id, name: 'Doge', status: '');
      var pet = await petApi.getPetById(id);
      expect(pet.name).toEqual('Doge');
    });

    it('updates existing pet', () async {
      var id = newId();
      var name = 'Snowy';

      await petApi.addPet(new Pet()..id = id);
      await petApi.updatePet(new Pet()
        ..id = id
        ..name = name);
      var pet = await petApi.getPetById(id);
      expect(pet.name).toEqual(name);
    });

    it('finds pets by status', () async {
      var id1 = newId();
      var id2 = newId();
      var id3 = newId();
      var status = 'available';

      return Future.wait([
        petApi.addPet(new Pet()
          ..id = id1
          ..status = status),
        petApi.addPet(new Pet()
          ..id = id2
          ..status = status),
        petApi.addPet(new Pet()
          ..id = id3
          ..status = 'sold')
      ]).then((_) async {
        var pets = await petApi.findPetsByStatus([status]);
        var petIds = pets.map((pet) => pet.id).toList();
        expect(petIds).toContain(id1);
        expect(petIds).toContain(id2);
        expect(petIds).not.toContain(id3);
      });
    });

    it('uploads a pet image', () async {
      var id = newId();
      await petApi.addPet(new Pet()..id = id);
      var file = new MultipartFile.fromBytes('file', [104, 101, 108, 108, 111]);
      await petApi.uploadFile(id, additionalMetadata: '', file: file);
    });
  });
}
