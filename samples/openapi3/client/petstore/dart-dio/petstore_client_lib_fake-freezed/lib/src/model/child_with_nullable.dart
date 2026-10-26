//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/parent_with_nullable.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'child_with_nullable.freezed.dart';
part 'child_with_nullable.g.dart';

// ignore_for_file: unused_import


@freezed
abstract class ChildWithNullable with _$ChildWithNullable {
  const factory ChildWithNullable({
    @JsonKey(
      name: r'type',
      required: false,
      includeIfNull: false,
    )
    String? type,
    @JsonKey(
      name: r'nullableProperty',
      required: false,
      includeIfNull: false,
    )
    String? nullableProperty,
    @JsonKey(
      name: r'otherProperty',
      required: false,
      includeIfNull: false,
    )
    String? otherProperty,
  }) = _ChildWithNullable;

  factory ChildWithNullable.fromJson(Map<String, dynamic> json) =>
      _$ChildWithNullableFromJson(json);
}

