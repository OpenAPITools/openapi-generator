@Skip('Needs real petstore')
import 'package:http/http.dart';
import 'package:openapi/api.dart';
import 'package:test/test.dart';

import 'random_id.dart';

void main() {
  var petApi = PetApi();

  Pet makePet({
    int id = 1234,
    String name = 'Fluffy',
    String status = '',
  }) {
    final category = Category()
      ..id = 1234
      ..name = 'eyeColor';
    final tags = [
      Tag()
        ..id = 1234
        ..name = 'New York',
      Tag()
        ..id = 124321
        ..name = 'Jose'
    ];

    return Pet(
        id: id,
        category: category,
        name: name, //required field
        tags: tags,
        photoUrls: ['https://petstore.com/sample/photo1.jpg'] //required field
        )
      ..status = PetStatusEnum.fromJson(status)
      ..photoUrls = ['https://petstore.com/sample/photo1.jpg'];
  }

  group('Pet API with live client', () {
    test('adds a new pet and gets it by id', () async {
      var id = newId();
      await petApi.addPet(makePet(id: id));
      var pet = await petApi.getPetById(id);
      expect(pet.id, equals(id));
    });

    test('doesn\'t get non-existing pet by id', () {
      expect(petApi.getPetById(newId()), throwsA(equals(TypeMatcher<ApiException>())));
    });

    test('deletes existing pet by id', () async {
      var id = newId();
      await petApi.addPet(makePet(id: id));
      await petApi.deletePet(id, apiKey: 'special-key');
      expect(petApi.getPetById(id), throwsA(equals(TypeMatcher<ApiException>())));
    });

    test('updates pet with form', () async {
      var id = newId();

      await petApi.addPet(makePet(id: id, name: 'Snowy'));
      await petApi.updatePetWithForm(id, name: 'Doge', status: '');
      var pet = await petApi.getPetById(id);
      expect(pet.name, equals('Doge'));
    });

    test('updates existing pet', () async {
      var id = newId();
      var name = 'Snowy';

      await petApi.addPet(makePet(id: id));
      await petApi.updatePet(makePet(id: id, name: name));
      var pet = await petApi.getPetById(id);
      expect(pet.name, equals(name));
    });

    test('finds pets by status', () async {
      var id1 = newId();
      var id2 = newId();
      var id3 = newId();
      var status = '${PetStatusEnum.available}';

      await petApi.addPet(makePet(id: id1, status: status));
      await petApi.addPet(makePet(id: id2, status: status));
      await petApi.addPet(makePet(id: id3, status: '${PetStatusEnum.sold}'));

      var pets = await petApi.findPetsByStatus([status]);
      var petIds = pets.map((pet) => pet.id).toList();
      expect(petIds, contains(id1));
      expect(petIds, contains(id2));
      expect(petIds, isNot(contains(id3)));
    });

    test('uploads a pet image', () async {
      var id = newId();
      await petApi.addPet(makePet(id: id));
      var file = new MultipartFile.fromBytes('file', [104, 101, 108, 108, 111]);
      await petApi.uploadFile(id, additionalMetadata: '', file: file);
    });
  });
}
