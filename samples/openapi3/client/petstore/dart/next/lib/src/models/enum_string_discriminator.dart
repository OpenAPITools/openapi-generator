// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'enum_string_discriminator.reflection.dart';
part 'enum_string_discriminator.serialization.dart';

//class defination

///
mixin EnumStringDiscriminatorMixin on $OpenApiObjectMixin {
  EnumStringDiscriminatorEnumStrTypeEnum get enumStrType;
}

///
class EnumStringDiscriminator
    with $OpenApiObjectMixin, EnumStringDiscriminatorMixin {
  @override
  EnumStringDiscriminatorEnumStrTypeEnum enumStrType;

  EnumStringDiscriminator.$all({
    required this.enumStrType,
  });

  EnumStringDiscriminator({
    required this.enumStrType,
  });
}

//inline enum def

extension type const EnumStringDiscriminatorEnumStrTypeEnum._(String value) {
  /// enum string type
  const EnumStringDiscriminatorEnumStrTypeEnum.a() : this._(r'type_a');

  /// enum string type
  const EnumStringDiscriminatorEnumStrTypeEnum.b() : this._(r'type_b');

  /// Creates a [EnumStringDiscriminatorEnumStrTypeEnum] enum from a value and safely checking if it exists.
  factory EnumStringDiscriminatorEnumStrTypeEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [EnumStringDiscriminatorEnumStrTypeEnum] enum from a value without checking if it exists.
  const EnumStringDiscriminatorEnumStrTypeEnum.$unsafe(String value)
      : this._(value);

  /// All possible values of the enum.
  static const List<EnumStringDiscriminatorEnumStrTypeEnum> values = [
    EnumStringDiscriminatorEnumStrTypeEnum.a(),
    EnumStringDiscriminatorEnumStrTypeEnum.b(),
  ];
}
