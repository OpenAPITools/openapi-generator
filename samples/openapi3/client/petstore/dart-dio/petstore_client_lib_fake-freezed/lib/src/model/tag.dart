//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'tag.freezed.dart';
part 'tag.g.dart';


@freezed
abstract class Tag with _$Tag {
  const factory Tag({
    @JsonKey(
      name: r'id',
      required: false,
      includeIfNull: false,
    )
    int? id,
    @JsonKey(
      name: r'name',
      required: false,
      includeIfNull: false,
    )
    String? name,
  }) = _Tag;

  factory Tag.fromJson(Map<String, dynamic> json) =>
      _$TagFromJson(json);
}

