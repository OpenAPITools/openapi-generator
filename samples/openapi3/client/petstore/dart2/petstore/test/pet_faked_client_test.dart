import 'dart:io';

import 'package:http/http.dart';
import 'package:openapi/api.dart';
import 'package:test/test.dart';

import 'fake_client.dart';
import 'random_id.dart';

void main() {
  final petApi = PetApi();

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
    return Pet(name: name)
      ..id = id
      ..category = category
      ..tags = tags
      ..status = PetStatusEnum.fromJson(status)
      ..photoUrls = ['https://petstore.com/sample/photo1.jpg'];
  }

  /// Setup the fake client then call [petApi.addPet]
  Future<dynamic> addPet(Pet pet) async {
    petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet',
        expectedPostRequestBody: await petApi.apiClient.serializeAsync(pet),
        postResponseBody: await petApi.apiClient.serializeAsync(pet),
        expectedHeaders: {'Content-Type': 'application/json'});
    return petApi.addPet(pet);
  }

  group('Pet API with faked client', () {
    test('adds a new pet and gets it by id', () async {
      final id = newId();
      final newPet = makePet(id: id);

      // use the pet api to add a pet
      petApi.apiClient.client = FakeClient(
          expectedUrl: 'http://petstore.swagger.io/v2/pet',
          expectedPostRequestBody: await petApi.apiClient.serializeAsync(newPet),
          postResponseBody: await petApi.apiClient.serializeAsync(newPet),
          expectedHeaders: {'Content-Type': 'application/json'});
      await petApi.addPet(newPet);

      // retrieve the same pet by id
      petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet/$id',
        getResponseBody: await petApi.apiClient.serializeAsync(newPet),
      );
      final retrievedPet = await petApi.getPetById(id);

      // check that the retrieved id is as expected
      expect(retrievedPet.id, equals(id));
    });

    test('doesn\'t get non-existing pet by id', () {
      final id = newId();
      petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet/$id',
        throwException: ApiException(400, 'not found'),
      );
      expect(petApi.getPetById(id), throwsA(equals(TypeMatcher<ApiException>())));
    });

    test('deletes existing pet by id', () async {
      final id = newId();
      Pet newPet = makePet(id: id);

      // add a new pet
      petApi.apiClient.client = FakeClient(
          expectedUrl: 'http://petstore.swagger.io/v2/pet',
          expectedPostRequestBody: await petApi.apiClient.serializeAsync(newPet),
          postResponseBody: await petApi.apiClient.serializeAsync(newPet),
          expectedHeaders: {'Content-Type': 'application/json'});
      await petApi.addPet(newPet);

      // delete the pet
      petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet/$id',
        expectedHeaders: {'api_key': 'special-key'},
        deleteResponseBody: '',
      );
      await petApi.deletePet(id, apiKey: 'special-key');

      // check for the deleted pet
      petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet/$id',
        throwException: ApiException(400, 'Not found'),
      );
      expect(petApi.getPetById(id), throwsA(equals(TypeMatcher<ApiException>())));
    });

    test('updates pet with form', () async {
      final id = newId();
      final newPet = makePet(id: id, name: 'Snowy');

      // add a new pet
      petApi.apiClient.client = FakeClient(
          expectedUrl: 'http://petstore.swagger.io/v2/pet',
          expectedPostRequestBody: await petApi.apiClient.serializeAsync(newPet),
          postResponseBody: await petApi.apiClient.serializeAsync(newPet),
          expectedHeaders: {'Content-Type': 'application/json'});
      await petApi.addPet(newPet);

      // update with form
      petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet/$id',
        expectedHeaders: {
          'Content-Type': 'application/x-www-form-urlencoded',
        },
        expectedPostRequestBody: {'name': 'Doge', 'status': ''},
        postResponseBody: '',
      );
      await petApi.updatePetWithForm(id, name: 'Doge', status: '');

      // check update worked
      newPet.name = 'Doge';
      petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet/$id',
        getResponseBody: await petApi.apiClient.serializeAsync(newPet),
      );
      final pet = await petApi.getPetById(id);
      expect(pet.name, equals('Doge'));
    });

    test('updates existing pet', () async {
      final id = newId();
      final name = 'Snowy';
      final newPet = makePet(id: id);
      final updatePet = makePet(id: id, name: name);

      // add a new pet
      petApi.apiClient.client = FakeClient(
          expectedUrl: 'http://petstore.swagger.io/v2/pet',
          expectedPostRequestBody: await petApi.apiClient.serializeAsync(newPet),
          postResponseBody: await petApi.apiClient.serializeAsync(newPet),
          expectedHeaders: {'Content-Type': 'application/json'});
      await petApi.addPet(newPet);

      // update the same pet
      petApi.apiClient.client = FakeClient(
          expectedUrl: 'http://petstore.swagger.io/v2/pet',
          expectedPutRequestBody: await petApi.apiClient.serializeAsync(updatePet),
          putResponseBody: await petApi.apiClient.serializeAsync(updatePet),
          expectedHeaders: {'Content-Type': 'application/json'});
      await petApi.updatePet(updatePet);

      // check update worked
      petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet/$id',
        getResponseBody: await petApi.apiClient.serializeAsync(updatePet),
      );
      final pet = await petApi.getPetById(id);
      expect(pet.name, equals(name));
    });

    test('finds pets by status', () async {
      final id1 = newId();
      final id2 = newId();
      final id3 = newId();
      final status = '${PetStatusEnum.available}';
      final pet1 = makePet(id: id1, status: status);
      final pet2 = makePet(id: id2, status: status);
      final pet3 = makePet(id: id3, status: '${PetStatusEnum.sold}');

      await addPet(pet1);
      await addPet(pet2);
      await addPet(pet3);

      // retrieve pets by status
      petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet/findByStatus?status=$status',
        getResponseBody: await petApi.apiClient.serializeAsync([pet1, pet2]),
      );
      final pets = await petApi.findPetsByStatus([status]);

      // tests serialisation and deserialisation of enum
      final petsByStatus = pets.where((p) => p.status == PetStatusEnum.available);
      expect(petsByStatus.length, equals(2));
      final petIds = pets.map((pet) => pet.id).toList();
      expect(petIds, contains(id1));
      expect(petIds, contains(id2));
      expect(petIds, isNot(contains(id3)));
    });

    test('uploads a pet image', () async {
      final id = newId();
      final newPet = makePet(id: id);
      // get some test data (recorded from live response)
      final uploadResponse = await File('test/file_upload_response.json').readAsString();

      // add a new pet
      petApi.apiClient.client = FakeClient(
          expectedUrl: 'http://petstore.swagger.io/v2/pet',
          expectedPostRequestBody: await petApi.apiClient.serializeAsync(newPet),
          postResponseBody: await petApi.apiClient.serializeAsync(newPet),
          expectedHeaders: {'Content-Type': 'application/json'});
      await petApi.addPet(newPet);

      petApi.apiClient.client = FakeClient(
        expectedUrl: 'http://petstore.swagger.io/v2/pet',
        sendResponseBody: uploadResponse,
      );
      final file = new MultipartFile.fromBytes('file', [104, 101, 108, 108, 111]);
      await petApi.uploadFile(id, file: file);
    });
  });
}
