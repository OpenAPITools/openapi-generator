//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'additional_properties_class.g.dart';

abstract class AdditionalPropertiesClass implements Built<AdditionalPropertiesClass, AdditionalPropertiesClassBuilder> {

    @nullable
    @BuiltValueField(wireName: r'map_property')
    BuiltMap<String, String> get mapProperty;

    @nullable
    @BuiltValueField(wireName: r'map_of_map_property')
    BuiltMap<String, BuiltMap<String, String>> get mapOfMapProperty;

    // Boilerplate code needed to wire-up generated code
    AdditionalPropertiesClass._();

    static void _initializeBuilder(AdditionalPropertiesClassBuilder b) => b;

    factory AdditionalPropertiesClass([void updates(AdditionalPropertiesClassBuilder b)]) = _$AdditionalPropertiesClass;
    static Serializer<AdditionalPropertiesClass> get serializer => _$additionalPropertiesClassSerializer;
}

