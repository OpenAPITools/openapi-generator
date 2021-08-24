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

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(NameBuilder b) => b;

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
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.name = valueDes;
                    break;
                case r'snake_case':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.snakeCase = valueDes;
                    break;
                case r'property':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.property = valueDes;
                    break;
                case r'123Number':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.n123number = valueDes;
                    break;
            }
        }
        return result.build();
    }
}

