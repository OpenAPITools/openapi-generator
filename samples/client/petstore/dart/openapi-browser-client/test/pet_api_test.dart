// ref: https://dart.dev/guides/testing
//

part of openapi.tests;


/// tests for PetApi
testPetApi {
  var instance = new PetApi();

  describe('tests for PetApi', () {
    // Add a new pet to the store
    //
    // 
    //
    //Future addPet(Pet body) async 
    it('test addPet', () async {
      // TODO
    });

    // Deletes a pet
    //
    // 
    //
    //Future deletePet(int petId, { String apiKey }) async 
    it('test deletePet', () async {
      // TODO
    });

    // Finds Pets by status
    //
    // Multiple status values can be provided with comma separated strings
    //
    //Future<List<Pet>> findPetsByStatus(List<String> status) async 
    it('test findPetsByStatus', () async {
      // TODO
    });

    // Finds Pets by tags
    //
    // Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    //
    //Future<List<Pet>> findPetsByTags(List<String> tags) async 
    it('test findPetsByTags', () async {
      // TODO
    });

    // Find pet by ID
    //
    // Returns a single pet
    //
    //Future<Pet> getPetById(int petId) async 
    it('test getPetById', () async {
      // TODO
    });

    // Update an existing pet
    //
    // 
    //
    //Future updatePet(Pet body) async 
    it('test updatePet', () async {
      // TODO
    });

    // Updates a pet in the store with form data
    //
    // 
    //
    //Future updatePetWithForm(int petId, { String name, String status }) async 
    it('test updatePetWithForm', () async {
      // TODO
    });

    // uploads an image
    //
    // 
    //
    //Future<ApiResponse> uploadFile(int petId, { String additionalMetadata, MultipartFile file }) async 
    it('test uploadFile', () async {
      // TODO
    });

  });
}
