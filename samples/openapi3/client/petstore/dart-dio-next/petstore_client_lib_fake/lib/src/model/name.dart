//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'name.g.dart';

/// Model for testing model name same as property name
///
/// Properties:
/// * [name] 
/// * [snakeCase] 
/// * [property] 
/// * [n123number] 
abstract class Name implements Built<Name, NameBuilder> {
    @BuiltValueField(wireName: r'name')
    int get name;

    @BuiltValueField(wireName: r'snake_case')
    int? get snakeCase;

    @BuiltValueField(wireName: r'property')
    String? get property;

    @BuiltValueField(wireName: r'123Number')
    int? get n123number;

    Name._();

    static void _initializeBuilder(NameBuilder b) => b;

    factory Name([void updates(NameBuilder b)]) = _$Name;

    @BuiltValueSerializer(custom: true)
    static Serializer<Name> get serializer => _$NameSerializer();
}

class _$NameSerializer implements StructuredSerializer<Name> {
    @override
    final Iterable<Type> types = const [Name, _$Name];

    @override
    final String wireName = r'Name';

    @override
    Iterable<Object?> serialize(Serializers serializers, Name object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        result
            ..add(r'name')
            ..add(serializers.serialize(object.name,
                specifiedType: const FullType(int)));
        if (object.snakeCase != null) {
            result
                ..add(r'snake_case')
                ..add(serializers.serialize(object.snakeCase,
                    specifiedType: const FullType(int)));
        }
        if (object.property != null) {
            result
                ..add(r'property')
                ..add(serializers.serialize(object.property,
                    specifiedType: const FullType(String)));
        }
        if (object.n123number != null) {
            result
                ..add(r'123Number')
                ..add(serializers.serialize(object.n123number,
                    specifiedType: const FullType(int)));
        }
        return result;
    }

    @override
    Name deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = NameBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            switch (key) {
                case r'name':
                    result.name = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'snake_case':
                    result.snakeCase = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'property':
                    result.property = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
                case r'123Number':
                    result.n123number = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
            }
        }
        return result.build();
    }
}

