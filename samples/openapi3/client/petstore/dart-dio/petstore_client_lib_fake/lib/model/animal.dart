import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'animal.g.dart';

abstract class Animal implements Built<Animal, AnimalBuilder> {

    @nullable
    @BuiltValueField(wireName: r'className')
    String get className;

    @nullable
    @BuiltValueField(wireName: r'color')
    String get color;

    // Boilerplate code needed to wire-up generated code
    Animal._();

    static void _initializeBuilder(AnimalBuilder b) => b
        ..color = 'red';

    factory Animal([updates(AnimalBuilder b)]) = _$Animal;
    static Serializer<Animal> get serializer => _$animalSerializer;
}

