//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/foo.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo_get_default_response.g.dart';

/// FooGetDefaultResponse
///
/// Properties:
/// * [string] 
abstract class FooGetDefaultResponse implements Built<FooGetDefaultResponse, FooGetDefaultResponseBuilder> {
    @BuiltValueField(wireName: r'string')
    Foo? get string;

    FooGetDefaultResponse._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(FooGetDefaultResponseBuilder b) => b;

    factory FooGetDefaultResponse([void updates(FooGetDefaultResponseBuilder b)]) = _$FooGetDefaultResponse;

    @BuiltValueSerializer(custom: true)
    static Serializer<FooGetDefaultResponse> get serializer => _$FooGetDefaultResponseSerializer();
}

class _$FooGetDefaultResponseSerializer implements StructuredSerializer<FooGetDefaultResponse> {
    @override
    final Iterable<Type> types = const [FooGetDefaultResponse, _$FooGetDefaultResponse];

    @override
    final String wireName = r'FooGetDefaultResponse';

    @override
    Iterable<Object?> serialize(Serializers serializers, FooGetDefaultResponse object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.string != null) {
            result
                ..add(r'string')
                ..add(serializers.serialize(object.string,
                    specifiedType: const FullType(Foo)));
        }
        return result;
    }

    @override
    FooGetDefaultResponse deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = FooGetDefaultResponseBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'string':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(Foo)) as Foo;
                    result.string.replace(valueDes);
                    break;
            }
        }
        return result.build();
    }
}

