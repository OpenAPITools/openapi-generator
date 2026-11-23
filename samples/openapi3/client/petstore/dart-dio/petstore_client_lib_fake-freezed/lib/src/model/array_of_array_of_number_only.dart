//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'array_of_array_of_number_only.freezed.dart';
part 'array_of_array_of_number_only.g.dart';


@freezed
abstract class ArrayOfArrayOfNumberOnly with _$ArrayOfArrayOfNumberOnly {
  const factory ArrayOfArrayOfNumberOnly({
    @JsonKey(
      name: r'ArrayArrayNumber',
      required: false,
      includeIfNull: false,
    )
    List<List<num>>? arrayArrayNumber,
  }) = _ArrayOfArrayOfNumberOnly;

  factory ArrayOfArrayOfNumberOnly.fromJson(Map<String, dynamic> json) =>
      _$ArrayOfArrayOfNumberOnlyFromJson(json);
}

