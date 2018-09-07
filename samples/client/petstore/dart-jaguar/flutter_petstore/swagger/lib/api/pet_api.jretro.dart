// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'pet_api.dart';

// **************************************************************************
// JaguarHttpGenerator
// **************************************************************************

abstract class _$PetApiClient implements ApiClient {
  final String basePath = "";
  Future<void> addPet(Pet body) async {
    var req = base.post.path(basePath).path("/pet").json(serializers.to(body));
    await req.go();
  }

  Future<void> deletePet(int petId, String apiKey) async {
    var req = base.delete
        .path(basePath)
        .path("/pet/:petId")
        .pathParams("petId", petId)
        .header("api_key", apiKey);
    await req.go();
  }

  Future<List<Pet>> findPetsByStatus(List<String> status) async {
    var req = base.get
        .path(basePath)
        .path("/pet/findByStatus")
        .query("status", status);
    return req.list(convert: serializers.oneFrom);
  }

  Future<List<Pet>> findPetsByTags(List<String> tags) async {
    var req =
        base.get.path(basePath).path("/pet/findByTags").query("tags", tags);
    return req.list(convert: serializers.oneFrom);
  }

  Future<Pet> getPetById(int petId) async {
    var req =
        base.get.path(basePath).path("/pet/:petId").pathParams("petId", petId);
    return req.one(convert: serializers.oneFrom);
  }

  Future<void> updatePet(Pet body) async {
    var req = base.put.path(basePath).path("/pet").json(serializers.to(body));
    await req.go();
  }

  Future<void> updatePetWithForm(int petId, String name, String status) async {
    var req = base.post
        .path(basePath)
        .path("/pet/:petId")
        .pathParams("petId", petId)
        .urlEncodedFormField(name, name)
        .urlEncodedFormField(status, status);
    await req.go();
  }

  Future<ApiResponse> uploadFile(
      int petId, String additionalMetadata, MultipartFile file) async {
    var req = base.post
        .path(basePath)
        .path("/pet/:petId/uploadImage")
        .pathParams("petId", petId)
        .multipart({"additionalMetadata": additionalMetadata}).multipart(
            {"file": file});
    return req.one(convert: serializers.oneFrom);
  }
}
