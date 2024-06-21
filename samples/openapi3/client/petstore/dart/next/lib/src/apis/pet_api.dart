import 'dart:convert';
import 'package:meta/meta.dart';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'pet_api.models.dart';

class PetApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> context;

  const PetApi({
    required this.networkingClient,
    required this.baseUrl,
    this.context = const {},
  });

  Future<PetApiAddPetResponse> addPet(
    PetApiAddPetRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiAddPetResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<PetApiDeletePetResponse> deletePet(
    PetApiDeletePetRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiDeletePetResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<PetApiFindPetsByStatusResponse> findPetsByStatus(
    PetApiFindPetsByStatusRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiFindPetsByStatusResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  @Deprecated('This operation has been deprecated')
  Future<PetApiFindPetsByTagsResponse> findPetsByTags(
    PetApiFindPetsByTagsRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiFindPetsByTagsResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<PetApiGetPetByIdResponse> getPetById(
    PetApiGetPetByIdRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiGetPetByIdResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<PetApiUpdatePetResponse> updatePet(
    PetApiUpdatePetRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiUpdatePetResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<PetApiUpdatePetWithFormResponse> updatePetWithForm(
    PetApiUpdatePetWithFormRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiUpdatePetWithFormResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<PetApiUploadFileResponse> uploadFile(
    PetApiUploadFileRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiUploadFileResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<PetApiUploadFileWithRequiredFileResponse> uploadFileWithRequiredFile(
    PetApiUploadFileWithRequiredFileRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return PetApiUploadFileWithRequiredFileResponse.fromResponse(
      response,
      context: newContext,
    );
  }
}
