//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'dog.dart';
import 'cat.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
part 'animal.g.dart';

/// Animal
///
/// Properties:
/// * [className] 
/// * [color] 
@BuiltValue(instantiable: false)
abstract class Animal  {
    @BuiltValueField(wireName: r'className')
    String get className;

    @BuiltValueField(wireName: r'color')
    String? get color;

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(AnimalBuilder b) => b
        ..color = 'red';

    @BuiltValueSerializer(custom: true)
    static StructuredSerializer<Animal> get serializer => _$AnimalSerializer();
}

class _$AnimalSerializer implements StructuredSerializer<Animal> {
    @override
    final Iterable<Type> types = const [Animal];

    @override
    final String wireName = r'Animal';

    @override
    Iterable<Object?> serialize(Serializers serializers, Animal object,
        {FullType specifiedType = FullType.unspecified}) {
        if (object is Cat) {
            final _serializer = Cat.serializer as StructuredSerializer<Cat>;
            return _serializer.serialize(serializers, object, specifiedType: FullType(Cat));
        }
        if (object is Dog) {
            final _serializer = Dog.serializer as StructuredSerializer<Dog>;
            return _serializer.serialize(serializers, object, specifiedType: FullType(Dog));
        }

        throw UnsupportedError('Discriminator not found for type ${object.runtimeType}');
    }

    @override
    Animal deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final serializedList = serialized.toList();
        final discIndex = serializedList.indexOf('className') + 1;
        final discValue = serializers.deserialize(serializedList[discIndex]).toString();
        switch (discValue) {
            case 'Cat':
                final _serializer = Cat.serializer as StructuredSerializer<Cat>;
                return _serializer.deserialize(serializers, serialized, specifiedType: FullType(Cat));
            case 'Dog':
                final _serializer = Dog.serializer as StructuredSerializer<Dog>;
                return _serializer.deserialize(serializers, serialized, specifiedType: FullType(Dog));
        }    
        throw UnsupportedError('Discriminator not found $discValue');
    }
}



