// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'pet_with_required_tags.reflection.dart';
part 'pet_with_required_tags.serialization.dart';

//class defination

///
mixin PetWithRequiredTagsMixin on $OpenApiObjectMixin {
  UndefinedWrapper<int> get id;
  UndefinedWrapper<Category> get category;
  String get name;
  List<String> get photoUrls;
  List<Tag> get tags;
  UndefinedWrapper<PetWithRequiredTagsStatusEnum> get status;
}

///
class PetWithRequiredTags with $OpenApiObjectMixin, PetWithRequiredTagsMixin {
  @override
  UndefinedWrapper<int> id;
  @override
  UndefinedWrapper<Category> category;
  @override
  String name;
  @override
  List<String> photoUrls;
  @override
  List<Tag> tags;
  @override
  UndefinedWrapper<PetWithRequiredTagsStatusEnum> status;

  PetWithRequiredTags.$all({
    required this.id,
    required this.category,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
  });

  PetWithRequiredTags({
    this.id = const UndefinedWrapper.undefined(),
    this.category = const UndefinedWrapper.undefined(),
    required this.name,
    required this.photoUrls,
    required this.tags,
    this.status = const UndefinedWrapper.undefined(),
  });
}

//inline enum def

extension type const PetWithRequiredTagsStatusEnum._(String value) {
  /// pet status in the store
  const PetWithRequiredTagsStatusEnum.available() : this._(r'available');

  /// pet status in the store
  const PetWithRequiredTagsStatusEnum.pending() : this._(r'pending');

  /// pet status in the store
  const PetWithRequiredTagsStatusEnum.sold() : this._(r'sold');

  /// Creates a [PetWithRequiredTagsStatusEnum] enum from a value and safely checking if it exists.
  factory PetWithRequiredTagsStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [PetWithRequiredTagsStatusEnum] enum from a value without checking if it exists.
  const PetWithRequiredTagsStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<PetWithRequiredTagsStatusEnum> values = [
    PetWithRequiredTagsStatusEnum.available(),
    PetWithRequiredTagsStatusEnum.pending(),
    PetWithRequiredTagsStatusEnum.sold(),
  ];
}
