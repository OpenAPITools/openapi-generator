//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/foo.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo_inline_response_default.g.dart';

/// FooInlineResponseDefault
///
/// Properties:
/// * [string] 
abstract class FooInlineResponseDefault implements Built<FooInlineResponseDefault, FooInlineResponseDefaultBuilder> {
    @BuiltValueField(wireName: r'string')
    Foo? get string;

    FooInlineResponseDefault._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(FooInlineResponseDefaultBuilder b) => b;

    factory FooInlineResponseDefault([void updates(FooInlineResponseDefaultBuilder b)]) = _$FooInlineResponseDefault;

    @BuiltValueSerializer(custom: true)
    static Serializer<FooInlineResponseDefault> get serializer => _$FooInlineResponseDefaultSerializer();
}

class _$FooInlineResponseDefaultSerializer implements StructuredSerializer<FooInlineResponseDefault> {
    @override
    final Iterable<Type> types = const [FooInlineResponseDefault, _$FooInlineResponseDefault];

    @override
    final String wireName = r'FooInlineResponseDefault';

    @override
    Iterable<Object?> serialize(Serializers serializers, FooInlineResponseDefault object,
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
    FooInlineResponseDefault deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = FooInlineResponseDefaultBuilder();

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

