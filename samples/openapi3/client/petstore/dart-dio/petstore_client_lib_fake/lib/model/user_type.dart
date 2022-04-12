//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'user_type.g.dart';

class UserType extends EnumClass {

  @BuiltValueEnumConst(wireName: r'admin')
  static const UserType admin = _$admin;
  @BuiltValueEnumConst(wireName: r'user')
  static const UserType user = _$user;

  static Serializer<UserType> get serializer => _$userTypeSerializer;

  const UserType._(String name): super(name);

  static BuiltSet<UserType> get values => _$values;
  static UserType valueOf(String name) => _$valueOf(name);
}

/// Optionally, enum_class can generate a mixin to go with your enum for use
/// with Angular. It exposes your enum constants as getters. So, if you mix it
/// in to your Dart component class, the values become available to the
/// corresponding Angular template.
///
/// Trigger mixin generation by writing a line like this one next to your enum.
abstract class UserTypeMixin = Object with _$UserTypeMixin;

