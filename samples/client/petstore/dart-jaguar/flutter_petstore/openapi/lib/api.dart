library openapi.api;

import 'package:http/io_client.dart';
import 'package:jaguar_serializer/jaguar_serializer.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';
import 'package:openapi/auth/api_key_auth.dart';
import 'package:openapi/auth/basic_auth.dart';
import 'package:openapi/auth/oauth.dart';
import 'package:jaguar_mimetype/jaguar_mimetype.dart';

import 'package:openapi/api/pet_api.dart';
import 'package:openapi/api/store_api.dart';
import 'package:openapi/api/user_api.dart';

import 'package:openapi/model/api_response.dart';
import 'package:openapi/model/category.dart';
import 'package:openapi/model/order.dart';
import 'package:openapi/model/pet.dart';
import 'package:openapi/model/tag.dart';
import 'package:openapi/model/user.dart';



final _jsonJaguarRepo = JsonRepo()
..add(ApiResponseSerializer())
..add(CategorySerializer())
..add(OrderSerializer())
..add(PetSerializer())
..add(TagSerializer())
..add(UserSerializer())
;
final Map<String, CodecRepo> _converters = {
    MimeTypes.json: _jsonJaguarRepo,
};



final _defaultInterceptors = [OAuthInterceptor(), BasicAuthInterceptor(), ApiKeyAuthInterceptor()];

class JaguarApiGen {
    List<Interceptor> interceptors;
    String basePath = "http://petstore.swagger.io/v2";
    Route _baseRoute;
    final Duration timeout;

    /**
    * Add custom global interceptors, put overrideInterceptors to true to set your interceptors only (auth interceptors will not be added)
    */
    JaguarApiGen({List<Interceptor> interceptors, bool overrideInterceptors = false, String baseUrl, this.timeout = const Duration(minutes: 2)}) {
        _baseRoute = Route(baseUrl ?? basePath).withClient(globalClient ?? IOClient());
        if(interceptors == null) {
            this.interceptors = _defaultInterceptors;
        }
        else if(overrideInterceptors){
            this.interceptors = interceptors;
        }
        else {
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
        (_defaultInterceptors[1] as BasicAuthInterceptor).authInfo[name] = BasicAuthInfo(username, password);
    }

    void setApiKey(String name, String apiKey) {
        (_defaultInterceptors[2] as ApiKeyAuthInterceptor).apiKeys[name] = apiKey;
    }

    
    /**
    * Get PetApi instance, base route and serializer can be overridden by a given but be careful,
    * by doing that all interceptors will not be executed
    */
    PetApi getPetApi({Route base, Map<String, CodecRepo> converters}) {
        if(base == null) {
            base = _baseRoute;
        }
        if(converters == null) {
            converters = _converters;
        }
        return PetApi(base: base, converters: converters, timeout: timeout);
    }

    
    /**
    * Get StoreApi instance, base route and serializer can be overridden by a given but be careful,
    * by doing that all interceptors will not be executed
    */
    StoreApi getStoreApi({Route base, Map<String, CodecRepo> converters}) {
        if(base == null) {
            base = _baseRoute;
        }
        if(converters == null) {
            converters = _converters;
        }
        return StoreApi(base: base, converters: converters, timeout: timeout);
    }

    
    /**
    * Get UserApi instance, base route and serializer can be overridden by a given but be careful,
    * by doing that all interceptors will not be executed
    */
    UserApi getUserApi({Route base, Map<String, CodecRepo> converters}) {
        if(base == null) {
            base = _baseRoute;
        }
        if(converters == null) {
            converters = _converters;
        }
        return UserApi(base: base, converters: converters, timeout: timeout);
    }

    
}