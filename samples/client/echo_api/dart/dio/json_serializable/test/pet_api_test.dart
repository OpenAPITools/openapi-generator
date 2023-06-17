import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

/// tests for PetApi
void main() {
  final instance = Openapi().getPetApi();

  group(PetApi, () {
    // Add a new pet to the store
    //
    //
    //
    //Future addPet(Pet pet) async
    test('test addPet', () async {});

    // Deletes a pet
    //
    //
    //
    //Future deletePet(int petId, { String apiKey }) async
    test('test deletePet', () async {});

    // Finds Pets by status
    //
    // Multiple status values can be provided with comma separated strings
    //
    //Future<List<Pet>> findPetsByStatus(List<String> status) async
    test('test findPetsByStatus', () async {});

    // Finds Pets by tags
    //
    // Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    //
    //Future<Set<Pet>> findPetsByTags(Set<String> tags) async
    test('test findPetsByTags', () async {});

    // Find pet by ID
    //
    // Returns a single pet
    //
    //Future<Pet> getPetById(int petId) async
    test('test getPetById', () async {});

    // Update an existing pet
    //
    //
    //
    //Future updatePet(Pet pet) async
    test('test updatePet', () async {});

    // Updates a pet in the store with form data
    //
    //
    //
    //Future updatePetWithForm(int petId, { String name, String status }) async
    test('test updatePetWithForm', () async {});

    // uploads an image
    //
    //
    //
    //Future<ApiResponse> uploadFile(int petId, { String additionalMetadata, MultipartFile file }) async
    test('test uploadFile', () async {});

    // uploads an image (required)
    //
    //
    //
    //Future<ApiResponse> uploadFileWithRequiredFile(int petId, MultipartFile requiredFile, { String additionalMetadata }) async
    test('test uploadFileWithRequiredFile', () async {});
  });
}
