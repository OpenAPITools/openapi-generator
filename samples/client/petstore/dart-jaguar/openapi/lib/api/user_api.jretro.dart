// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'user_api.dart';

// **************************************************************************
// JaguarHttpGenerator
// **************************************************************************

abstract class _$UserApiClient implements ApiClient {
  final String basePath = "";
  Future<void> createUser(User user) async {
    var req = base.post.path(basePath).path("/user").json(serializers.to(user));
    await req.go();
  }

  Future<void> createUsersWithArrayInput(List<User> user) async {
    var req = base.post
        .path(basePath)
        .path("/user/createWithArray")
        .json(serializers.to(user));
    await req.go();
  }

  Future<void> createUsersWithListInput(List<User> user) async {
    var req = base.post
        .path(basePath)
        .path("/user/createWithList")
        .json(serializers.to(user));
    await req.go();
  }

  Future<void> deleteUser(String username) async {
    var req = base.delete
        .path(basePath)
        .path("/user/:username")
        .pathParams("username", username);
    await req.go();
  }

  Future<User> getUserByName(String username) async {
    var req = base.get
        .path(basePath)
        .path("/user/:username")
        .pathParams("username", username);
    return req.one(convert: serializers.oneFrom);
  }

  Future<String> loginUser(String username, String password) async {
    var req = base.get
        .path(basePath)
        .path("/user/login")
        .query("username", username)
        .query("password", password);
    return req.one();
  }

  Future<void> logoutUser() async {
    var req = base.get.path(basePath).path("/user/logout");
    await req.go();
  }

  Future<void> updateUser(String username, User user) async {
    var req = base.put
        .path(basePath)
        .path("/user/:username")
        .pathParams("username", username)
        .json(serializers.to(user));
    await req.go();
  }
}
