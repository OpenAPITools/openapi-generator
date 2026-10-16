//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'has_only_read_only.freezed.dart';
part 'has_only_read_only.g.dart';


@freezed
abstract class HasOnlyReadOnly with _$HasOnlyReadOnly {
  const factory HasOnlyReadOnly({
    @JsonKey(
      name: r'bar',
      required: false,
      includeIfNull: false,
    )
    String? bar,
    @JsonKey(
      name: r'foo',
      required: false,
      includeIfNull: false,
    )
    String? foo,
  }) = _HasOnlyReadOnly;

  factory HasOnlyReadOnly.fromJson(Map<String, dynamic> json) =>
      _$HasOnlyReadOnlyFromJson(json);
}

