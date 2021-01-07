//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:openapi/model/animal.dart';
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'mixed_properties_and_additional_properties_class.g.dart';

abstract class MixedPropertiesAndAdditionalPropertiesClass implements Built<MixedPropertiesAndAdditionalPropertiesClass, MixedPropertiesAndAdditionalPropertiesClassBuilder> {

    @nullable
    @BuiltValueField(wireName: r'uuid')
    String get uuid;

    @nullable
    @BuiltValueField(wireName: r'dateTime')
    DateTime get dateTime;

    @nullable
    @BuiltValueField(wireName: r'map')
    BuiltMap<String, Animal> get map;

    // Boilerplate code needed to wire-up generated code
    MixedPropertiesAndAdditionalPropertiesClass._();

    static void _initializeBuilder(MixedPropertiesAndAdditionalPropertiesClassBuilder b) => b;

    factory MixedPropertiesAndAdditionalPropertiesClass([void updates(MixedPropertiesAndAdditionalPropertiesClassBuilder b)]) = _$MixedPropertiesAndAdditionalPropertiesClass;
    static Serializer<MixedPropertiesAndAdditionalPropertiesClass> get serializer => _$mixedPropertiesAndAdditionalPropertiesClassSerializer;
}

