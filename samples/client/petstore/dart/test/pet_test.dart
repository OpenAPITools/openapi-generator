part of tests;

testPetApi() {
  var petApi = new PetApi();

  describe('Pet API ', () {
    it('adds a new pet and gets it by id', () async {
      var id = 137;

      await petApi.addPet(new Pet()..id = id);
      var pet = await petApi.getPetById(id);
      expect(pet.id).toEqual(id);
    });

    it('doesn\'t get non-existing pet by id', () {
      expect(petApi.getPetById(6789099)).toThrowWith(anInstanceOf: ApiException);
    });

    it('deletes existing pet by id', () async {
      var id = 7689;
      await petApi.addPet(new Pet()..id = id);
      await petApi.deletePet(id, 'special-key');
      expect(petApi.getPetById(id)).toThrowWith(anInstanceOf: ApiException);
    });

    it('updates pet with form', () async {
      var id = 52341;
      await petApi.addPet(new Pet()..id = id..name='Snowy');
      await petApi.updatePetWithForm('$id', 'Doge', '');
      var pet = await petApi.getPetById(id);
      expect(pet.name).toEqual('Doge');
    });

    it('updates existing pet', () async {
      var id = 900001;
      var name = 'Snowy';

      await petApi.addPet(new Pet()..id = id);
      await petApi.updatePet(new Pet()..id = id..name = name);
      var pet = await petApi.getPetById(id);
      expect(pet.name).toEqual(name);
    });

    it('finds pets by status', () async {
      var id1 = 754111;
      var id2 = 1231341;
      var id3 = 6776251;
      var status = 'available';

      return Future.wait([petApi.addPet(new Pet()..id = id1..status = status),
                   petApi.addPet(new Pet()..id = id2..status = status),
                   petApi.addPet(new Pet()..id = id3..status = 'sold')])
            .then((_) async {

        var pets = await petApi.findPetsByStatus([status]);
        var petIds = pets.map((pet) => pet.id).toList();
        expect(petIds).toContain(id1);
        expect(petIds).toContain(id2);
        expect(petIds).not.toContain(id3);
      });
    });

    it('finds pets by tag', () async {
      var snowyId = 253156;
      var grumpyId = 734215;
      var snowyTags = [new Tag()..id=12211..name='terrier'];
      var grumpyTags = [new Tag()..id=455803..name='grumpy'];
      await petApi.addPet(new Pet()..id = snowyId..name = 'Snowy'..tags = snowyTags);
      await petApi.addPet(new Pet()..id = grumpyId..name = 'Grumpy Cat'..tags = grumpyTags);

      var pets = await petApi.findPetsByTags(['grumpy']);
      var petIds = pets.map((pet) => pet.id).toList();
      expect(petIds).toContain(grumpyId);
      expect(petIds).not.toContain(snowyId);
    });

    it('uploads a pet image', () async {
      var id = 672322;
      await petApi.addPet(new Pet()..id = id);
      var file = new MultipartFile.fromBytes('file', [104, 101, 108, 108, 111]);
      await petApi.uploadFile(id, '', file);
    });

  });
}