//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'api_response.g.dart';

/// ApiResponse
///
/// Properties:
/// * [code] 
/// * [type] 
/// * [message] 
abstract class ApiResponse implements Built<ApiResponse, ApiResponseBuilder> {
    @BuiltValueField(wireName: r'code')
    int? get code;

    @BuiltValueField(wireName: r'type')
    String? get type;

    @BuiltValueField(wireName: r'message')
    String? get message;

    ApiResponse._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(ApiResponseBuilder b) => b;

    factory ApiResponse([void updates(ApiResponseBuilder b)]) = _$ApiResponse;

    @BuiltValueSerializer(custom: true)
    static Serializer<ApiResponse> get serializer => _$ApiResponseSerializer();
}

class _$ApiResponseSerializer implements StructuredSerializer<ApiResponse> {
    @override
    final Iterable<Type> types = const [ApiResponse, _$ApiResponse];

    @override
    final String wireName = r'ApiResponse';

    @override
    Iterable<Object?> serialize(Serializers serializers, ApiResponse object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.code != null) {
            result
                ..add(r'code')
                ..add(serializers.serialize(object.code,
                    specifiedType: const FullType(int)));
        }
        if (object.type != null) {
            result
                ..add(r'type')
                ..add(serializers.serialize(object.type,
                    specifiedType: const FullType(String)));
        }
        if (object.message != null) {
            result
                ..add(r'message')
                ..add(serializers.serialize(object.message,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    ApiResponse deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ApiResponseBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'code':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.code = valueDes;
                    break;
                case r'type':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.type = valueDes;
                    break;
                case r'message':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.message = valueDes;
                    break;
            }
        }
        return result.build();
    }
}

