//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/read_only_first.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'array_test.freezed.dart';
part 'array_test.g.dart';


@freezed
abstract class ArrayTest with _$ArrayTest {
  const factory ArrayTest({
    @JsonKey(
      name: r'array_of_string',
      required: false,
      includeIfNull: false,
    )
    List<String>? arrayOfString,
    @JsonKey(
      name: r'array_array_of_integer',
      required: false,
      includeIfNull: false,
    )
    List<List<int>>? arrayArrayOfInteger,
    @JsonKey(
      name: r'array_array_of_model',
      required: false,
      includeIfNull: false,
    )
    List<List<ReadOnlyFirst>>? arrayArrayOfModel,
  }) = _ArrayTest;

  factory ArrayTest.fromJson(Map<String, dynamic> json) =>
      _$ArrayTestFromJson(json);
}

