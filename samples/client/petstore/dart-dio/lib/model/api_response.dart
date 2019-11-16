        import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'api_response.g.dart';

abstract class ApiResponse implements Built<ApiResponse, ApiResponseBuilder> {

    
        @nullable

    
    @BuiltValueField(wireName: 'code')
    int get code;
    
        @nullable

    
    @BuiltValueField(wireName: 'type')
    String get type;
    
        @nullable

    
    @BuiltValueField(wireName: 'message')
    String get message;

    // Boilerplate code needed to wire-up generated code
    ApiResponse._();

    factory ApiResponse([updates(ApiResponseBuilder b)]) = _$ApiResponse;
    static Serializer<ApiResponse> get serializer => _$apiResponseSerializer;

}

