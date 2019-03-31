// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'pet_api.dart';

// **************************************************************************
// JaguarHttpGenerator
// **************************************************************************

abstract class _$PetApiClient implements ApiClient {
  final String basePath = "";
  Future<void> addPet(Pet body) async {
    var req = base.post
        .metadata({
          "auth": [
            {
              "type": "oauth2",
              "name": "petstore_auth",
            }
          ],
        })
        .path(basePath)
        .path("/pet")
        .json(jsonConverter.to(body));
    await req.go(throwOnErr: true);
  }

  Future<void> deletePet(int petId, String apiKey) async {
    var req = base.delete
        .metadata({
          "auth": [
            {
              "type": "oauth2",
              "name": "petstore_auth",
            }
          ],
        })
        .path(basePath)
        .path("/pet/:petId")
        .pathParams("petId", petId)
        .header("api_key", apiKey);
    await req.go(throwOnErr: true);
  }

  Future<List<Pet>> findPetsByStatus(List<String> status) async {
    var req = base.get
        .metadata({
          "auth": [
            {
              "type": "oauth2",
              "name": "petstore_auth",
            }
          ],
        })
        .path(basePath)
        .path("/pet/findByStatus")
        .query("status", status);
    return req.go(throwOnErr: true).then(decodeList);
  }

  Future<List<Pet>> findPetsByTags(List<String> tags) async {
    var req = base.get
        .metadata({
          "auth": [
            {
              "type": "oauth2",
              "name": "petstore_auth",
            }
          ],
        })
        .path(basePath)
        .path("/pet/findByTags")
        .query("tags", tags);
    return req.go(throwOnErr: true).then(decodeList);
  }

  Future<Pet> getPetById(int petId) async {
    var req = base.get
        .metadata({
          "auth": [
            {
              "type": "apiKey",
              "name": "api_key",
              "keyName": "api_key",
              "where": "header",
            }
          ],
        })
        .path(basePath)
        .path("/pet/:petId")
        .pathParams("petId", petId);
    return req.go(throwOnErr: true).then(decodeOne);
  }

  Future<void> updatePet(Pet body) async {
    var req = base.put
        .metadata({
          "auth": [
            {
              "type": "oauth2",
              "name": "petstore_auth",
            }
          ],
        })
        .path(basePath)
        .path("/pet")
        .json(jsonConverter.to(body));
    await req.go(throwOnErr: true);
  }

  Future<void> updatePetWithForm(int petId, String name, String status) async {
    var req = base.post
        .metadata({
          "auth": [
            {
              "type": "oauth2",
              "name": "petstore_auth",
            }
          ],
        })
        .path(basePath)
        .path("/pet/:petId")
        .pathParams("petId", petId)
        .urlEncodedFormField(name, name)
        .urlEncodedFormField(status, status);
    await req.go(throwOnErr: true);
  }

  Future<ApiResponse> uploadFile(
      int petId, String additionalMetadata, MultipartFile file) async {
    var req = base.post
        .metadata({
          "auth": [
            {
              "type": "oauth2",
              "name": "petstore_auth",
            }
          ],
        })
        .path(basePath)
        .path("/pet/:petId/uploadImage")
        .pathParams("petId", petId)
        .multipart({"additionalMetadata": additionalMetadata})
        .multipart({"file": file});
    return req.go(throwOnErr: true).then(decodeOne);
  }
}
