// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'pet_ref.reflection.dart';
part 'pet_ref.serialization.dart';

//class defination

///
mixin PetRefMixin on $OpenApiObjectMixin {
  UndefinedWrapper<int> get id;
  UndefinedWrapper<Category> get category;
  String get name;
  List<String> get photoUrls;
  UndefinedWrapper<List<Tag>> get tags;
  UndefinedWrapper<PetRefStatusEnum> get status;
}

///
class PetRef with $OpenApiObjectMixin, PetRefMixin {
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
  UndefinedWrapper<PetRefStatusEnum> status;

  PetRef.$all({
    required this.id,
    required this.category,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
  });

  PetRef({
    this.id = const UndefinedWrapper.undefined(),
    this.category = const UndefinedWrapper.undefined(),
    required this.name,
    required this.photoUrls,
    this.tags = const UndefinedWrapper.undefined(),
    this.status = const UndefinedWrapper.undefined(),
  });
}

//inline enum def

extension type const PetRefStatusEnum._(String value) {
  /// pet status in the store
  const PetRefStatusEnum.available() : this._(r'available');

  /// pet status in the store
  const PetRefStatusEnum.pending() : this._(r'pending');

  /// pet status in the store
  const PetRefStatusEnum.sold() : this._(r'sold');

  /// Creates a [PetRefStatusEnum] enum from a value and safely checking if it exists.
  factory PetRefStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [PetRefStatusEnum] enum from a value without checking if it exists.
  const PetRefStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<PetRefStatusEnum> values = [
    PetRefStatusEnum.available(),
    PetRefStatusEnum.pending(),
    PetRefStatusEnum.sold(),
  ];
}
