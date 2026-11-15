//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'additional_properties_class.freezed.dart';
part 'additional_properties_class.g.dart';


@freezed
abstract class AdditionalPropertiesClass with _$AdditionalPropertiesClass {
  const factory AdditionalPropertiesClass({
    @JsonKey(
      name: r'map_property',
      required: false,
      includeIfNull: false,
    )
    Map<String, String>? mapProperty,
    @JsonKey(
      name: r'map_of_map_property',
      required: false,
      includeIfNull: false,
    )
    Map<String, Map<String, String>>? mapOfMapProperty,
    @JsonKey(
      name: r'map_of_array_integer',
      required: false,
      includeIfNull: false,
    )
    Map<String, List<int>>? mapOfArrayInteger,
  }) = _AdditionalPropertiesClass;

  factory AdditionalPropertiesClass.fromJson(Map<String, dynamic> json) =>
      _$AdditionalPropertiesClassFromJson(json);
}

