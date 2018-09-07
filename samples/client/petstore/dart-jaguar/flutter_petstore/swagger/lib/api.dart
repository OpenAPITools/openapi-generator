library swagger.api;

import 'package:jaguar_serializer/jaguar_serializer.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';

import 'package:swagger/api/pet_api.dart';
import 'package:swagger/api/store_api.dart';
import 'package:swagger/api/user_api.dart';

import 'package:swagger/model/amount.dart';
import 'package:swagger/model/api_response.dart';
import 'package:swagger/model/category.dart';
import 'package:swagger/model/currency.dart';
import 'package:swagger/model/order.dart';
import 'package:swagger/model/pet.dart';
import 'package:swagger/model/tag.dart';
import 'package:swagger/model/user.dart';


final jsonJaguarRepo = JsonRepo()
..add(AmountSerializer())
..add(ApiResponseSerializer())
..add(CategorySerializer())
..add(CurrencySerializer())
..add(OrderSerializer())
..add(PetSerializer())
..add(TagSerializer())
..add(UserSerializer())
;

final baseSwaggerPath = "http://petstore.swagger.io/v2";
final _baseRoute = Route(baseSwaggerPath);

class SwaggerGen {
    final List<Interceptor> interceptors;
    SwaggerGen({this.interceptors = const []}) {
        interceptors.forEach((interceptor) {
            _baseRoute.before(interceptor.before);
            _baseRoute.after(interceptor.after);
        });
    }

    PetApi getPetApi({Route base, SerializerRepo serializers}) {
        if(base == null) {
            base = _baseRoute;
        }
        if(serializers == null) {
            serializers = jsonJaguarRepo;
        }
        return PetApi(base: base, serializers: serializers);
    }

    StoreApi getStoreApi({Route base, SerializerRepo serializers}) {
        if(base == null) {
            base = _baseRoute;
        }
        if(serializers == null) {
            serializers = jsonJaguarRepo;
        }
        return StoreApi(base: base, serializers: serializers);
    }

    UserApi getUserApi({Route base, SerializerRepo serializers}) {
        if(base == null) {
            base = _baseRoute;
        }
        if(serializers == null) {
            serializers = jsonJaguarRepo;
        }
        return UserApi(base: base, serializers: serializers);
    }

    
}