import 'dart:convert';
import 'package:meta/meta.dart';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'user_api.models.dart';

class UserApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> context;

  const UserApi({
    required this.networkingClient,
    required this.baseUrl,
    this.context = const {},
  });

  Future<UserApiCreateUserResponse> createUser(
    UserApiCreateUserRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiCreateUserResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<UserApiCreateUsersWithArrayInputResponse> createUsersWithArrayInput(
    UserApiCreateUsersWithArrayInputRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiCreateUsersWithArrayInputResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<UserApiCreateUsersWithListInputResponse> createUsersWithListInput(
    UserApiCreateUsersWithListInputRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiCreateUsersWithListInputResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<UserApiDeleteUserResponse> deleteUser(
    UserApiDeleteUserRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiDeleteUserResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<UserApiGetUserByNameResponse> getUserByName(
    UserApiGetUserByNameRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiGetUserByNameResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<UserApiLoginUserResponse> loginUser(
    UserApiLoginUserRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiLoginUserResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<UserApiLogoutUserResponse> logoutUser(
    UserApiLogoutUserRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiLogoutUserResponse.fromResponse(
      response,
      context: newContext,
    );
  }
  Future<UserApiUpdateUserResponse> updateUser(
    UserApiUpdateUserRequest request, {
    Map<String, dynamic> context = const {},
  }) async {
    final newContext = {...this.context, ...context};
    final httpRequest = await request.createHttpRequest(
      context: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiUpdateUserResponse.fromResponse(
      response,
      context: newContext,
    );
  }
}
