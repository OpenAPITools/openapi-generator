//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'deprecated_object.freezed.dart';
part 'deprecated_object.g.dart';


@Deprecated('DeprecatedObject has been deprecated')
@freezed
abstract class DeprecatedObject with _$DeprecatedObject {
  const factory DeprecatedObject({
    @JsonKey(
      name: r'name',
      required: false,
      includeIfNull: false,
    )
    String? name,
  }) = _DeprecatedObject;

  factory DeprecatedObject.fromJson(Map<String, dynamic> json) =>
      _$DeprecatedObjectFromJson(json);
}

