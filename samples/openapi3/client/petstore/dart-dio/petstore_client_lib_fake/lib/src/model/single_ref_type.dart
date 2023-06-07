//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'single_ref_type.g.dart';

class SingleRefType extends EnumClass {

  @BuiltValueEnumConst(wireName: r'admin')
  static const SingleRefType admin = _$admin;
  @BuiltValueEnumConst(wireName: r'user')
  static const SingleRefType user = _$user;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const SingleRefType unknownDefaultOpenApi = _$unknownDefaultOpenApi;

  static Serializer<SingleRefType> get serializer => _$singleRefTypeSerializer;

  const SingleRefType._(String name): super(name);

  static BuiltSet<SingleRefType> get values => _$values;
  static SingleRefType valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class SingleRefTypeMixin = Object with _$SingleRefTypeMixin;

