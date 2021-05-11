//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:openapi/model/foo.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_response_default.g.dart';

abstract class InlineResponseDefault implements Built<InlineResponseDefault, InlineResponseDefaultBuilder> {

    @nullable
    @BuiltValueField(wireName: r'string')
    Foo get string;

    InlineResponseDefault._();

    static void _initializeBuilder(InlineResponseDefaultBuilder b) => b;

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
    Iterable<Object> serialize(Serializers serializers, InlineResponseDefault object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.string != null) {
            result
                ..add(r'string')
                ..add(serializers.serialize(object.string,
                    specifiedType: const FullType(Foo)));
        }
        return result;
    }

    @override
    InlineResponseDefault deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = InlineResponseDefaultBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'string':
                    result.string.replace(serializers.deserialize(value,
                        specifiedType: const FullType(Foo)) as Foo);
                    break;
            }
        }
        return result.build();
    }
}

