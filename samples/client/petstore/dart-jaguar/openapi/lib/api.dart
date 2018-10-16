library openapi.api;

import 'package:http/http.dart';
import 'package:jaguar_serializer/jaguar_serializer.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';
import 'package:openapi/auth/api_key_auth.dart';
import 'package:openapi/auth/basic_auth.dart';
import 'package:openapi/auth/oauth.dart';

import 'package:openapi/api/pet_api.dart';
import 'package:openapi/api/store_api.dart';
import 'package:openapi/api/user_api.dart';

import 'package:openapi/model/api_response.dart';
import 'package:openapi/model/category.dart';
import 'package:openapi/model/order.dart';
import 'package:openapi/model/pet.dart';
import 'package:openapi/model/tag.dart';
import 'package:openapi/model/user.dart';

final jsonJaguarRepo = JsonRepo()
  ..add(ApiResponseSerializer())
  ..add(CategorySerializer())
  ..add(OrderSerializer())
  ..add(PetSerializer())
  ..add(TagSerializer())
  ..add(UserSerializer());

final _defaultInterceptors = [
  OAuthInterceptor(),
  BasicAuthInterceptor(),
  ApiKeyAuthInterceptor()
];

class JaguarApiGen {
  List<Interceptor> interceptors;
  String basePath = "http://petstore.swagger.io/v2";
  Route _baseRoute;

  /**
    * Add custom global interceptors, put overrideInterceptors to true to set your interceptors only (auth interceptors will not be added)
    */
  JaguarApiGen(
      {List<Interceptor> interceptors,
      bool overrideInterceptors = false,
      String baseUrl}) {
    _baseRoute =
        Route(baseUrl ?? basePath).withClient(globalClient ?? IOClient());
    if (interceptors == null) {
      this.interceptors = _defaultInterceptors;
    } else if (overrideInterceptors) {
      this.interceptors = interceptors;
    } else {
      this.interceptors = List.from(_defaultInterceptors)..addAll(interceptors);
    }

    this.interceptors.forEach((interceptor) {
      _baseRoute.before(interceptor.before);
      _baseRoute.after(interceptor.after);
    });
  }

  void setOAuthToken(String name, String token) {
    (_defaultInterceptors[0] as OAuthInterceptor).tokens[name] = token;
  }

  void setBasicAuth(String name, String username, String password) {
    (_defaultInterceptors[1] as BasicAuthInterceptor).authInfo[name] =
        BasicAuthInfo(username, password);
  }

  void setApiKey(String name, String apiKey) {
    (_defaultInterceptors[2] as ApiKeyAuthInterceptor).apiKeys[name] = apiKey;
  }

  /**
    * Get PetApi instance, base route and serializer can be overridden by a given but be careful,
    * by doing that all interceptors will not be executed
    */
  PetApi getPetApi({Route base, SerializerRepo serializers}) {
    if (base == null) {
      base = _baseRoute;
    }
    if (serializers == null) {
      serializers = jsonJaguarRepo;
    }
    return PetApi(base: base, serializers: serializers);
  }

  /**
    * Get StoreApi instance, base route and serializer can be overridden by a given but be careful,
    * by doing that all interceptors will not be executed
    */
  StoreApi getStoreApi({Route base, SerializerRepo serializers}) {
    if (base == null) {
      base = _baseRoute;
    }
    if (serializers == null) {
      serializers = jsonJaguarRepo;
    }
    return StoreApi(base: base, serializers: serializers);
  }

  /**
    * Get UserApi instance, base route and serializer can be overridden by a given but be careful,
    * by doing that all interceptors will not be executed
    */
  UserApi getUserApi({Route base, SerializerRepo serializers}) {
    if (base == null) {
      base = _baseRoute;
    }
    if (serializers == null) {
      serializers = jsonJaguarRepo;
    }
    return UserApi(base: base, serializers: serializers);
  }
}
