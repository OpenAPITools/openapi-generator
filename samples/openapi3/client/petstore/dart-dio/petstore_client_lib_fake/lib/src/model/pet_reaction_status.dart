//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'pet_reaction_status.g.dart';

class PetReactionStatus extends EnumClass {

  @BuiltValueEnumConst(wireName: r'liked')
  static const PetReactionStatus liked = _$liked;
  @BuiltValueEnumConst(wireName: r'disliked')
  static const PetReactionStatus disliked = _$disliked;
  @BuiltValueEnumConst(wireName: r'barked')
  static const PetReactionStatus barked = _$barked;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const PetReactionStatus unknownDefaultOpenApi = _$unknownDefaultOpenApi;

  static Serializer<PetReactionStatus> get serializer => _$petReactionStatusSerializer;

  const PetReactionStatus._(String name): super(name);

  static BuiltSet<PetReactionStatus> get values => _$values;
  static PetReactionStatus valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class PetReactionStatusMixin = Object with _$PetReactionStatusMixin;

