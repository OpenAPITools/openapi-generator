//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'parent_with_nullable.freezed.dart';
part 'parent_with_nullable.g.dart';


@freezed
abstract class ParentWithNullable with _$ParentWithNullable {
  const factory ParentWithNullable({
    @JsonKey(
      name: r'type',
      includeIfNull: false,
    )
    String? type,
    @JsonKey(
      name: r'nullableProperty',
      includeIfNull: false,
    )
    String? nullableProperty,
  }) = _ParentWithNullable;

  factory ParentWithNullable.fromJson(Map<String, dynamic> json) =>
      _$ParentWithNullableFromJson(json);
}

