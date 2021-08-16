//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/foo.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_response_default.g.dart';

/// InlineResponseDefault
///
/// Properties:
/// * [string] 
abstract class InlineResponseDefault implements Built<InlineResponseDefault, InlineResponseDefaultBuilder> {
    @BuiltValueField(wireName: r'string')
    Foo? get string;

    InlineResponseDefault._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(InlineResponseDefaultBuilder b) => b;

    factory InlineResponseDefault([void updates(InlineResponseDefaultBuilder b)]) = _$InlineResponseDefault;

    @BuiltValueSerializer(custom: true)
    static Serializer<InlineResponseDefault> get serializer => _$InlineResponseDefaultSerializer();
}

class _$InlineResponseDefaultSerializer implements StructuredSerializer<InlineResponseDefault> {
    @override
    final Iterable<Type> types = const [InlineResponseDefault, _$InlineResponseDefault];

    @override
    final String wireName = r'InlineResponseDefault';

    @override
    Iterable<Object?> serialize(Serializers serializers, InlineResponseDefault object,
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
    InlineResponseDefault deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = InlineResponseDefaultBuilder();

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

