import 'package:petstore_api/_internal.dart';

part 'user_api.requests.dart';
part 'user_api.responses.dart';

class UserApi {
  final NetworkingClientBase networkingClient;
  final Uri baseUrl;
  final Map<String, dynamic> userContext;

  const UserApi({
    required this.networkingClient,
    required this.baseUrl,
    this.userContext = const {},
  });

  Future<UserApiCreateUserResponse> createUser(
    UserApiCreateUserRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiCreateUserResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<UserApiCreateUsersWithArrayInputResponse> createUsersWithArrayInput(
    UserApiCreateUsersWithArrayInputRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiCreateUsersWithArrayInputResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<UserApiCreateUsersWithListInputResponse> createUsersWithListInput(
    UserApiCreateUsersWithListInputRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiCreateUsersWithListInputResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<UserApiDeleteUserResponse> deleteUser(
    UserApiDeleteUserRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiDeleteUserResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<UserApiGetUserByNameResponse> getUserByName(
    UserApiGetUserByNameRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiGetUserByNameResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<UserApiLoginUserResponse> loginUser(
    UserApiLoginUserRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiLoginUserResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<UserApiLogoutUserResponse> logoutUser(
    UserApiLogoutUserRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiLogoutUserResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
  Future<UserApiUpdateUserResponse> updateUser(
    UserApiUpdateUserRequest request, {
    Map<String, dynamic> userContext = const {},
  }) async {
    final newContext = {...this.userContext, ...userContext};
    final httpRequest = await request.createHttpRequest(
      userContext: newContext,
      baseUrl: baseUrl,
    );
    final response = await networkingClient.sendRequest(httpRequest);
    return UserApiUpdateUserResponse.fromResponse(
      response,
      userContext: newContext,
      wireSerializationOptions: request.wireSerializationOptions,
    );
  }
}
