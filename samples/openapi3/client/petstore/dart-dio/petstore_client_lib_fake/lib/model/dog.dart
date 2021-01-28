//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:openapi/model/animal.dart';
import 'package:openapi/model/dog_all_of.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'dog.g.dart';

abstract class Dog implements Built<Dog, DogBuilder> {

    @nullable
    @BuiltValueField(wireName: r'className')
    String get className;

    @nullable
    @BuiltValueField(wireName: r'color')
    String get color;

    @nullable
    @BuiltValueField(wireName: r'breed')
    String get breed;

    // Boilerplate code needed to wire-up generated code
    Dog._();

    static void _initializeBuilder(DogBuilder b) => b
        ..color = 'red';

    factory Dog([void updates(DogBuilder b)]) = _$Dog;
    static Serializer<Dog> get serializer => _$dogSerializer;
}

