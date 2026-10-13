//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:dio/dio.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'format_test.freezed.dart';
part 'format_test.g.dart';


@freezed
abstract class FormatTest with _$FormatTest {
  const factory FormatTest({
    @JsonKey(
      name: r'integer',
      includeIfNull: false,
    )
    int? integer,
    @JsonKey(
      name: r'int32',
      includeIfNull: false,
    )
    int? int32,
    @JsonKey(
      name: r'int64',
      includeIfNull: false,
    )
    int? int64,
    @JsonKey(
      name: r'number',
      includeIfNull: false,
    )
    required num number,
    @JsonKey(
      name: r'float',
      includeIfNull: false,
    )
    double? float,
    @JsonKey(
      name: r'double',
      includeIfNull: false,
    )
    double? double_,
    @JsonKey(
      name: r'decimal',
      includeIfNull: false,
    )
    double? decimal,
    @JsonKey(
      name: r'string',
      includeIfNull: false,
    )
    String? string,
    @JsonKey(
      name: r'byte',
      includeIfNull: false,
    )
    required String byte,
    @JsonKey(includeFromJson: false, includeToJson: false)
    MultipartFile? binary,
    @JsonKey(
      name: r'date',
      includeIfNull: false,
    )
    required DateTime date,
    @JsonKey(
      name: r'dateTime',
      includeIfNull: false,
    )
    DateTime? dateTime,
    @JsonKey(
      name: r'uuid',
      includeIfNull: false,
    )
    String? uuid,
    @JsonKey(
      name: r'password',
      includeIfNull: false,
    )
    required String password,
    /// A string that is a 10 digit number. Can have leading zeros.
    @JsonKey(
      name: r'pattern_with_digits',
      includeIfNull: false,
    )
    String? patternWithDigits,
    /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
    @JsonKey(
      name: r'pattern_with_digits_and_delimiter',
      includeIfNull: false,
    )
    String? patternWithDigitsAndDelimiter,
  }) = _FormatTest;

  factory FormatTest.fromJson(Map<String, dynamic> json) =>
      _$FormatTestFromJson(json);
}

