//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'dog_all_of.g.dart';

abstract class DogAllOf implements Built<DogAllOf, DogAllOfBuilder> {

    @nullable
    @BuiltValueField(wireName: r'breed')
    String get breed;

    // Boilerplate code needed to wire-up generated code
    DogAllOf._();

    static void _initializeBuilder(DogAllOfBuilder b) => b;

    factory DogAllOf([void updates(DogAllOfBuilder b)]) = _$DogAllOf;
    static Serializer<DogAllOf> get serializer => _$dogAllOfSerializer;
}

