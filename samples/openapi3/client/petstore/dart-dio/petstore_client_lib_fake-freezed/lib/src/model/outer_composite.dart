//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'outer_composite.freezed.dart';
part 'outer_composite.g.dart';


@freezed
abstract class OuterComposite with _$OuterComposite {
  const factory OuterComposite({
    @JsonKey(
      name: r'my_number',
      required: false,
      includeIfNull: false,
    )
    num? myNumber,
    @JsonKey(
      name: r'my_string',
      required: false,
      includeIfNull: false,
    )
    String? myString,
    @JsonKey(
      name: r'my_boolean',
      required: false,
      includeIfNull: false,
    )
    bool? myBoolean,
  }) = _OuterComposite;

  factory OuterComposite.fromJson(Map<String, dynamic> json) =>
      _$OuterCompositeFromJson(json);
}

