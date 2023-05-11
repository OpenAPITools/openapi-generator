//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:dio/dio.dart';
import 'repository_base.dart';
import 'repository_impl.dart';
import 'package:openapi/api.dart';
import 'package:openapi/models.dart';
import 'package:openapi/src/auth/_exports.dart';

class Openapi {
  static const String basePath = r'http://petstore.swagger.io:80/v2';

  final Dio dio;
  final SerializationRepositoryBase serializationRepository;


  Openapi({
    Dio? dio,
    SerializationRepositoryBase? serializationRepositoryOverride,
    String? basePathOverride,
    List<Interceptor>? interceptors,
  })  : this.dio = dio ??
            Dio(BaseOptions(
              baseUrl: basePathOverride ?? basePath,
              connectTimeout: const Duration(milliseconds: 5000),
              receiveTimeout: const Duration(milliseconds: 3000),
            )), this.serializationRepository = serializationRepositoryOverride ?? const JsonSerializableRepository() {
    if (interceptors == null) {
      this.dio.interceptors.addAll([
        OAuthInterceptor(),
        BasicAuthInterceptor(),
        BearerAuthInterceptor(),
        ApiKeyAuthInterceptor(),
      ]);
    } else {
      this.dio.interceptors.addAll(interceptors);
    }
  }

  void setOAuthToken(String name, String token) {
    if (this.dio.interceptors.any((i) => i is OAuthInterceptor)) {
      (this.dio.interceptors.firstWhere((i) => i is OAuthInterceptor) as OAuthInterceptor).tokens[name] = token;
    }
  }

  void setBearerAuth(String name, String token) {
    if (this.dio.interceptors.any((i) => i is BearerAuthInterceptor)) {
      (this.dio.interceptors.firstWhere((i) => i is BearerAuthInterceptor) as BearerAuthInterceptor).tokens[name] = token;
    }
  }

  void setBasicAuth(String name, String username, String password) {
    if (this.dio.interceptors.any((i) => i is BasicAuthInterceptor)) {
      (this.dio.interceptors.firstWhere((i) => i is BasicAuthInterceptor) as BasicAuthInterceptor).authInfo[name] = BasicAuthInfo(username, password);
    }
  }

  void setApiKey(String name, String apiKey) {
    if (this.dio.interceptors.any((i) => i is ApiKeyAuthInterceptor)) {
      (this.dio.interceptors.firstWhere((element) => element is ApiKeyAuthInterceptor) as ApiKeyAuthInterceptor).apiKeys[name] = apiKey;
    }
  }

  /// Get AnotherFakeApi instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  AnotherFakeApi getAnotherFakeApi() {
    return AnotherFakeApi(AnotherFakeApiRaw(dio), serializationRepository);
  }

  /// Get DefaultApi instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  DefaultApi getDefaultApi() {
    return DefaultApi(DefaultApiRaw(dio), serializationRepository);
  }

  /// Get FakeApi instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  FakeApi getFakeApi() {
    return FakeApi(FakeApiRaw(dio), serializationRepository);
  }

  /// Get FakeClassnameTags123Api instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  FakeClassnameTags123Api getFakeClassnameTags123Api() {
    return FakeClassnameTags123Api(FakeClassnameTags123ApiRaw(dio), serializationRepository);
  }

  /// Get PetApi instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  PetApi getPetApi() {
    return PetApi(PetApiRaw(dio), serializationRepository);
  }

  /// Get StoreApi instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  StoreApi getStoreApi() {
    return StoreApi(StoreApiRaw(dio), serializationRepository);
  }

  /// Get UserApi instance, base route and serializer can be overridden by a given but be careful,
  /// by doing that all interceptors will not be executed
  UserApi getUserApi() {
    return UserApi(UserApiRaw(dio), serializationRepository);
  }
}
