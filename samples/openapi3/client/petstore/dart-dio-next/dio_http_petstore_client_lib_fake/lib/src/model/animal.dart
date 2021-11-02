//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'animal.g.dart';

/// Animal
///
/// Properties:
/// * [className] 
/// * [color] 
abstract class Animal implements Built<Animal, AnimalBuilder> {
    @BuiltValueField(wireName: r'className')
    String get className;

    @BuiltValueField(wireName: r'color')
    String? get color;

    Animal._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(AnimalBuilder b) => b
        ..color = 'red';

    factory Animal([void updates(AnimalBuilder b)]) = _$Animal;

    @BuiltValueSerializer(custom: true)
    static Serializer<Animal> get serializer => _$AnimalSerializer();
}

class _$AnimalSerializer implements StructuredSerializer<Animal> {
    @override
    final Iterable<Type> types = const [Animal, _$Animal];

    @override
    final String wireName = r'Animal';

    @override
    Iterable<Object?> serialize(Serializers serializers, Animal object,
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
        return result;
    }

    @override
    Animal deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = AnimalBuilder();

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
            }
        }
        return result.build();
    }
}

