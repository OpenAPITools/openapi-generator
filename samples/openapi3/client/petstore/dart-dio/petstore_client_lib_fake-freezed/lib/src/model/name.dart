//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'name.freezed.dart';
part 'name.g.dart';


@freezed
abstract class Name with _$Name {
  const factory Name({
    @JsonKey(
      name: r'name',
      required: true,
      includeIfNull: false,
    )
    required int name,
    @JsonKey(
      name: r'snake_case',
      required: false,
      includeIfNull: false,
    )
    int? snakeCase,
    @JsonKey(
      name: r'property',
      required: false,
      includeIfNull: false,
    )
    String? property,
    @JsonKey(
      name: r'123Number',
      required: false,
      includeIfNull: false,
    )
    int? n123number,
  }) = _Name;

  factory Name.fromJson(Map<String, dynamic> json) =>
      _$NameFromJson(json);
}

