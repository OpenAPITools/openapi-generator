//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo.dart';
import 'package:json_annotation/json_annotation.dart';

part 'foo_basic_get_default_response.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FooBasicGetDefaultResponse {
  /// Returns a new [FooBasicGetDefaultResponse] instance.
  FooBasicGetDefaultResponse({
    this.string,
  });

  @JsonKey(name: r'string', required: false, includeIfNull: false)
  final Foo? string;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FooBasicGetDefaultResponse && other.string == string;

  @override
  int get hashCode => string.hashCode;

  factory FooBasicGetDefaultResponse.fromJson(Map<String, dynamic> json) =>
      _$FooBasicGetDefaultResponseFromJson(json);

  Map<String, dynamic> toJson() => _$FooBasicGetDefaultResponseToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
