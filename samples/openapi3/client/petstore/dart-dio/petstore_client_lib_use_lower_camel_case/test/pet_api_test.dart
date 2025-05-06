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
    test('test addPet', () async {
      // TODO
    });

    // Deletes a pet
    //
    // 
    //
    //Future deletePet(int petid, { String apiKey }) async
    test('test deletePet', () async {
      // TODO
    });

    // Finds Pets by status
    //
    // Multiple status values can be provided with comma separated strings
    //
    //Future<List<Pet>> findPetsByStatus(List<String> status) async
    test('test findPetsByStatus', () async {
      // TODO
    });

    // Finds Pets by tags
    //
    // Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    //
    //Future<Set<Pet>> findPetsByTags(Set<String> tags) async
    test('test findPetsByTags', () async {
      // TODO
    });

    // Find pet by ID
    //
    // Returns a single pet
    //
    //Future<Pet> getPetById(int petid) async
    test('test getPetById', () async {
      // TODO
    });

    // Update an existing pet
    //
    // 
    //
    //Future updatePet(Pet pet) async
    test('test updatePet', () async {
      // TODO
    });

    // Updates a pet in the store with form data
    //
    // 
    //
    //Future updatePetWithForm(int petid, { String name, String status }) async
    test('test updatePetWithForm', () async {
      // TODO
    });

    // uploads an image
    //
    // 
    //
    //Future<ApiResponse> uploadFile(int petid, { String additionalmetadata, MultipartFile file }) async
    test('test uploadFile', () async {
      // TODO
    });

    // uploads an image (required)
    //
    // 
    //
    //Future<ApiResponse> uploadFileWithRequiredFile(int petid, MultipartFile requiredfile, { String additionalmetadata }) async
    test('test uploadFileWithRequiredFile', () async {
      // TODO
    });

  });
}
