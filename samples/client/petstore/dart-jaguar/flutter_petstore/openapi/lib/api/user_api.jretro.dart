// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'user_api.dart';

// **************************************************************************
// JaguarHttpGenerator
// **************************************************************************

abstract class _$UserApiClient implements ApiClient {
  final String basePath = "";
  Future<void> createUser(User body) async {
    var req =
        base.post.path(basePath).path("/user").json(jsonConverter.to(body));
    await req.go(throwOnErr: true);
  }

  Future<void> createUsersWithArrayInput(List<User> body) async {
    var req = base.post
        .path(basePath)
        .path("/user/createWithArray")
        .json(jsonConverter.to(body));
    await req.go(throwOnErr: true);
  }

  Future<void> createUsersWithListInput(List<User> body) async {
    var req = base.post
        .path(basePath)
        .path("/user/createWithList")
        .json(jsonConverter.to(body));
    await req.go(throwOnErr: true);
  }

  Future<void> deleteUser(String username) async {
    var req = base.delete
        .path(basePath)
        .path("/user/:username")
        .pathParams("username", username);
    await req.go(throwOnErr: true);
  }

  Future<User> getUserByName(String username) async {
    var req = base.get
        .path(basePath)
        .path("/user/:username")
        .pathParams("username", username);
    return req.go(throwOnErr: true).then(decodeOne);
  }

  Future<String> loginUser(String username, String password) async {
    var req = base.get
        .path(basePath)
        .path("/user/login")
        .query("username", username)
        .query("password", password);
    return req.go(throwOnErr: true).then(decodeOne);
  }

  Future<void> logoutUser() async {
    var req = base.get.path(basePath).path("/user/logout");
    await req.go(throwOnErr: true);
  }

  Future<void> updateUser(String username, User body) async {
    var req = base.put
        .path(basePath)
        .path("/user/:username")
        .pathParams("username", username)
        .json(jsonConverter.to(body));
    await req.go(throwOnErr: true);
  }
}
