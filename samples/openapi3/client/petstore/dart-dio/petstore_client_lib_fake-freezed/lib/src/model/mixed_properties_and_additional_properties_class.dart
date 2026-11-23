//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/animal.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'mixed_properties_and_additional_properties_class.freezed.dart';
part 'mixed_properties_and_additional_properties_class.g.dart';


@freezed
abstract class MixedPropertiesAndAdditionalPropertiesClass with _$MixedPropertiesAndAdditionalPropertiesClass {
  const factory MixedPropertiesAndAdditionalPropertiesClass({
    @JsonKey(
      name: r'uuid',
      required: false,
      includeIfNull: false,
    )
    String? uuid,
    @JsonKey(
      name: r'dateTime',
      required: false,
      includeIfNull: false,
    )
    DateTime? dateTime,
    @JsonKey(
      name: r'map',
      required: false,
      includeIfNull: false,
    )
    Map<String, Animal>? map,
  }) = _MixedPropertiesAndAdditionalPropertiesClass;

  factory MixedPropertiesAndAdditionalPropertiesClass.fromJson(Map<String, dynamic> json) =>
      _$MixedPropertiesAndAdditionalPropertiesClassFromJson(json);
}

