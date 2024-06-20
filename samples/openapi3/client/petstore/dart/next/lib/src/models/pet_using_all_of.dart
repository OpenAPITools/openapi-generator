// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'pet_using_all_of.reflection.dart';
part 'pet_using_all_of.serialization.dart';

//class defination

///
mixin PetUsingAllOfMixin on $OpenApiObjectMixin {
  UndefinedWrapper<int> get id;
  UndefinedWrapper<Category> get category;
  String get name;
  List<String> get photoUrls;
  UndefinedWrapper<List<Tag>> get tags;
  UndefinedWrapper<PetUsingAllOfStatusEnum> get status;
}

///
class PetUsingAllOf with $OpenApiObjectMixin, PetUsingAllOfMixin {
  @override
  UndefinedWrapper<int> id;
  @override
  UndefinedWrapper<Category> category;
  @override
  String name;
  @override
  List<String> photoUrls;
  @override
  UndefinedWrapper<List<Tag>> tags;
  @override
  UndefinedWrapper<PetUsingAllOfStatusEnum> status;

  PetUsingAllOf.$all({
    required this.id,
    required this.category,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
  });

  PetUsingAllOf({
    this.id = const UndefinedWrapper.undefined(),
    this.category = const UndefinedWrapper.undefined(),
    required this.name,
    required this.photoUrls,
    this.tags = const UndefinedWrapper.undefined(),
    this.status = const UndefinedWrapper.undefined(),
  });
}

//inline enum def

extension type const PetUsingAllOfStatusEnum._(String value) {
  /// pet status in the store
  const PetUsingAllOfStatusEnum.available() : this._(r'available');

  /// pet status in the store
  const PetUsingAllOfStatusEnum.pending() : this._(r'pending');

  /// pet status in the store
  const PetUsingAllOfStatusEnum.sold() : this._(r'sold');

  /// Creates a [PetUsingAllOfStatusEnum] enum from a value and safely checking if it exists.
  factory PetUsingAllOfStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [PetUsingAllOfStatusEnum] enum from a value without checking if it exists.
  const PetUsingAllOfStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<PetUsingAllOfStatusEnum> values = [
    PetUsingAllOfStatusEnum.available(),
    PetUsingAllOfStatusEnum.pending(),
    PetUsingAllOfStatusEnum.sold(),
  ];
}
