//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/category.dart';
import 'package:openapi/src/model/tag.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'pet.freezed.dart';
part 'pet.g.dart';


@freezed
abstract class Pet with _$Pet {
  const factory Pet({
    @JsonKey(
      name: r'id',
      required: false,
      includeIfNull: false,
    )
    int? id,
    @JsonKey(
      name: r'category',
      required: false,
      includeIfNull: false,
    )
    Category? category,
    @JsonKey(
      name: r'name',
      required: true,
      includeIfNull: false,
    )
    required String name,
    @JsonKey(
      name: r'photoUrls',
      required: true,
      includeIfNull: false,
    )
    required Set<String> photoUrls,
    @JsonKey(
      name: r'tags',
      required: false,
      includeIfNull: false,
    )
    List<Tag>? tags,
    /// pet status in the store
    @JsonKey(
      name: r'status',
      required: false,
      includeIfNull: false,
      unknownEnumValue: PetStatusEnum.unknownDefaultOpenApi,
    )
    PetStatusEnum? status,
  }) = _Pet;

  factory Pet.fromJson(Map<String, dynamic> json) =>
      _$PetFromJson(json);
}

/// pet status in the store
enum PetStatusEnum {
@JsonValue(r'available')
available(r'available'),
@JsonValue(r'pending')
pending(r'pending'),
@JsonValue(r'sold')
sold(r'sold'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const PetStatusEnum(this.value);

final String value;

@override
String toString() => value;
}


