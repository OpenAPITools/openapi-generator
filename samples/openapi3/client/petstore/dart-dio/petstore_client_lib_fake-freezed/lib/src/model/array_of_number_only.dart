//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'array_of_number_only.freezed.dart';
part 'array_of_number_only.g.dart';


@freezed
abstract class ArrayOfNumberOnly with _$ArrayOfNumberOnly {
  const factory ArrayOfNumberOnly({
    @JsonKey(
      name: r'ArrayNumber',
      required: false,
      includeIfNull: false,
    )
    List<num>? arrayNumber,
  }) = _ArrayOfNumberOnly;

  factory ArrayOfNumberOnly.fromJson(Map<String, dynamic> json) =>
      _$ArrayOfNumberOnlyFromJson(json);
}

