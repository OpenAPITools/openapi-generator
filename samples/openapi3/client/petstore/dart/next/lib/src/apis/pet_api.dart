import 'package:petstore_api/_internal.dart';

part 'pet_api.requests.dart';
part 'pet_api.responses.dart';

class PetApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> userContext;

  const PetApi({
    required this.networkingClient,
    required this.baseUrl,
    this.userContext = const {},
  });

  Future<PetApiAddPetResponse> addPet(
    PetApiAddPetRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiAddPetResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<PetApiDeletePetResponse> deletePet(
    PetApiDeletePetRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiDeletePetResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<PetApiFindPetsByStatusResponse> findPetsByStatus(
    PetApiFindPetsByStatusRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiFindPetsByStatusResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  @Deprecated('This operation has been deprecated')
  Future<PetApiFindPetsByTagsResponse> findPetsByTags(
    PetApiFindPetsByTagsRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiFindPetsByTagsResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<PetApiGetPetByIdResponse> getPetById(
    PetApiGetPetByIdRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiGetPetByIdResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<PetApiUpdatePetResponse> updatePet(
    PetApiUpdatePetRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiUpdatePetResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<PetApiUpdatePetWithFormResponse> updatePetWithForm(
    PetApiUpdatePetWithFormRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiUpdatePetWithFormResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<PetApiUploadFileResponse> uploadFile(
    PetApiUploadFileRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiUploadFileResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<PetApiUploadFileWithRequiredFileResponse> uploadFileWithRequiredFile(
    PetApiUploadFileWithRequiredFileRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiUploadFileWithRequiredFileResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
}
