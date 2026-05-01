//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'dart:core';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';
import 'package:one_of/one_of.dart';

part 'pet_empty_metadata.g.dart';

/// Reproduces a bug where a schema that resolves to a class with zero properties (e.g. an empty oneOf wrapper) emitted invalid Dart syntax in `==`, `hashCode`, and the named-args constructor. 
@BuiltValue()
abstract class PetEmptyMetadata implements Built<PetEmptyMetadata, PetEmptyMetadataBuilder> {
  /// One Of [String], [int]
  OneOf get oneOf;

  PetEmptyMetadata._();

  factory PetEmptyMetadata([void updates(PetEmptyMetadataBuilder b)]) = _$PetEmptyMetadata;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(PetEmptyMetadataBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<PetEmptyMetadata> get serializer => _$PetEmptyMetadataSerializer();
}

class _$PetEmptyMetadataSerializer implements PrimitiveSerializer<PetEmptyMetadata> {
  @override
  final Iterable<Type> types = const [PetEmptyMetadata, _$PetEmptyMetadata];

  @override
  final String wireName = r'PetEmptyMetadata';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    PetEmptyMetadata object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
  }

  @override
  Object serialize(
    Serializers serializers,
    PetEmptyMetadata object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final oneOf = object.oneOf;
    return serializers.serialize(oneOf.value, specifiedType: FullType(oneOf.valueType))!;
  }

  @override
  PetEmptyMetadata deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = PetEmptyMetadataBuilder();
    Object? oneOfDataSrc;
    final targetType = const FullType(OneOf, [FullType(String), FullType(int), ]);
    oneOfDataSrc = serialized;
    result.oneOf = serializers.deserialize(oneOfDataSrc, specifiedType: targetType) as OneOf;
    return result.build();
  }
}

