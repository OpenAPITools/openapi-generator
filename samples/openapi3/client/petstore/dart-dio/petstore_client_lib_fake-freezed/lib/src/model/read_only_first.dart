//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'read_only_first.freezed.dart';
part 'read_only_first.g.dart';


@freezed
abstract class ReadOnlyFirst with _$ReadOnlyFirst {
  const factory ReadOnlyFirst({
    @JsonKey(
      name: r'bar',
      required: false,
      includeIfNull: false,
    )
    String? bar,
    @JsonKey(
      name: r'baz',
      required: false,
      includeIfNull: false,
    )
    String? baz,
  }) = _ReadOnlyFirst;

  factory ReadOnlyFirst.fromJson(Map<String, dynamic> json) =>
      _$ReadOnlyFirstFromJson(json);
}

