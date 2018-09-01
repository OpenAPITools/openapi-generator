import 'dart:async';

import 'package:http/http.dart';
import 'package:openapi/api.dart';
import 'package:test/test.dart';

import 'random_id.dart';

void main() {
  var petApi = new PetApi();

  group('Pet API ', () {
    test('adds a new pet and gets it by id', () async {
      var id = newId();

      await petApi.addPet(new Pet()..id = id);
      var pet = await petApi.getPetById(id);
      expect(pet.id, equals(id));
    });

    test('doesn\'t get non-existing pet by id', () {
      expect(petApi.getPetById(newId()), throwsA(equals(TypeMatcher<ApiException>())));
    });

    test('deletes existing pet by id', () async {
      var id = newId();
      await petApi.addPet(new Pet()..id = id);
      await petApi.deletePet(id, apiKey: 'special-key');
      expect(petApi.getPetById(id), throwsA(equals(TypeMatcher<ApiException>())));
    });

    test('updates pet with form', () async {
      var id = newId();
      await petApi.addPet(new Pet()
        ..id = id
        ..name = 'Snowy');
      await petApi.updatePetWithForm(id, name: 'Doge', status: '');
      var pet = await petApi.getPetById(id);
      expect(pet.name, equals('Doge'));
    });

    test('updates existing pet', () async {
      var id = newId();
      var name = 'Snowy';

      await petApi.addPet(new Pet()..id = id);
      await petApi.updatePet(new Pet()
        ..id = id
        ..name = name);
      var pet = await petApi.getPetById(id);
      expect(pet.name, equals(name));
    });

    test('finds pets by status', () async {
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
        expect(petIds, contains(id1));
        expect(petIds, contains(id2));
        expect(petIds, isNot(contains(id3)));
      });
    });

    test('uploads a pet image', () async {
      var id = newId();
      await petApi.addPet(new Pet()..id = id);
      var file = new MultipartFile.fromBytes('file', [104, 101, 108, 108, 111]);
      await petApi.uploadFile(id, additionalMetadata: '', file: file);
    });
  });
}
