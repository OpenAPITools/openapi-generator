//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'number_only.freezed.dart';
part 'number_only.g.dart';


@freezed
abstract class NumberOnly with _$NumberOnly {
  const factory NumberOnly({
    @JsonKey(
      name: r'JustNumber',
      required: false,
      includeIfNull: false,
    )
    num? justNumber,
  }) = _NumberOnly;

  factory NumberOnly.fromJson(Map<String, dynamic> json) =>
      _$NumberOnlyFromJson(json);
}

