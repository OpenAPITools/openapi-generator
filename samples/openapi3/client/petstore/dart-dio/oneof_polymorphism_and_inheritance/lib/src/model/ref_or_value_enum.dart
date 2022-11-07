//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'ref_or_value_enum.g.dart';

class RefOrValueEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'REF')
  static const RefOrValueEnum REF = _$REF;
  @BuiltValueEnumConst(wireName: r'VALUE')
  static const RefOrValueEnum VALUE = _$VALUE;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const RefOrValueEnum unknownDefaultOpenApi = _$unknownDefaultOpenApi;

  static Serializer<RefOrValueEnum> get serializer => _$refOrValueEnumSerializer;

  const RefOrValueEnum._(String name): super(name);

  static BuiltSet<RefOrValueEnum> get values => _$values;
  static RefOrValueEnum valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class RefOrValueEnumMixin = Object with _$RefOrValueEnumMixin;

