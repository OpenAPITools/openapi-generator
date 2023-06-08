//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'read_only_first.g.dart';

/// ReadOnlyFirst
///
/// Properties:
/// * [bar] 
/// * [baz] 
abstract class ReadOnlyFirst implements Built<ReadOnlyFirst, ReadOnlyFirstBuilder> {
    @BuiltValueField(wireName: r'bar')
    String? get bar;

    @BuiltValueField(wireName: r'baz')
    String? get baz;

    ReadOnlyFirst._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(ReadOnlyFirstBuilder b) => b;

    factory ReadOnlyFirst([void updates(ReadOnlyFirstBuilder b)]) = _$ReadOnlyFirst;

    @BuiltValueSerializer(custom: true)
    static Serializer<ReadOnlyFirst> get serializer => _$ReadOnlyFirstSerializer();
}

class _$ReadOnlyFirstSerializer implements StructuredSerializer<ReadOnlyFirst> {
    @override
    final Iterable<Type> types = const [ReadOnlyFirst, _$ReadOnlyFirst];

    @override
    final String wireName = r'ReadOnlyFirst';

    @override
    Iterable<Object?> serialize(Serializers serializers, ReadOnlyFirst object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.bar != null) {
            result
                ..add(r'bar')
                ..add(serializers.serialize(object.bar,
                    specifiedType: const FullType(String)));
        }
        if (object.baz != null) {
            result
                ..add(r'baz')
                ..add(serializers.serialize(object.baz,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    ReadOnlyFirst deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ReadOnlyFirstBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'bar':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.bar = valueDes;
                    break;
                case r'baz':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.baz = valueDes;
                    break;
            }
        }
        return result.build();
    }
}

