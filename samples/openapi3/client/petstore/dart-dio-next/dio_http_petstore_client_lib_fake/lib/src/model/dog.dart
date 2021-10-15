//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/dog_all_of.dart';
import 'package:openapi/src/model/animal.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'dog.g.dart';

// ignore_for_file: unused_import

/// Dog
///
/// Properties:
/// * [className] 
/// * [color] 
/// * [breed] 
abstract class Dog implements Built<Dog, DogBuilder> {
    @BuiltValueField(wireName: r'className')
    String get className;

    @BuiltValueField(wireName: r'color')
    String? get color;

    @BuiltValueField(wireName: r'breed')
    String? get breed;

    Dog._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(DogBuilder b) => b
        ..color = 'red';

    factory Dog([void updates(DogBuilder b)]) = _$Dog;

    @BuiltValueSerializer(custom: true)
    static Serializer<Dog> get serializer => _$DogSerializer();
}

class _$DogSerializer implements StructuredSerializer<Dog> {
    @override
    final Iterable<Type> types = const [Dog, _$Dog];

    @override
    final String wireName = r'Dog';

    @override
    Iterable<Object?> serialize(Serializers serializers, Dog object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        result
            ..add(r'className')
            ..add(serializers.serialize(object.className,
                specifiedType: const FullType(String)));
        if (object.color != null) {
            result
                ..add(r'color')
                ..add(serializers.serialize(object.color,
                    specifiedType: const FullType(String)));
        }
        if (object.breed != null) {
            result
                ..add(r'breed')
                ..add(serializers.serialize(object.breed,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    Dog deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = DogBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'className':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.className = valueDes;
                    break;
                case r'color':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.color = valueDes;
                    break;
                case r'breed':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.breed = valueDes;
                    break;
            }
        }
        return result.build();
    }
}

