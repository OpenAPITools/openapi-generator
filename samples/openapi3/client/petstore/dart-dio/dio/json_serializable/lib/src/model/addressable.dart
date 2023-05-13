//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'addressable.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Addressable {
  /// Returns a new [Addressable] instance.
  Addressable({
    this.href,
    this.id,
  });

  /// Hyperlink reference
  @JsonKey(name: r'href', required: false, includeIfNull: false)
  final String? href;

  /// unique identifier
  @JsonKey(name: r'id', required: false, includeIfNull: false)
  final String? id;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Addressable && other.href == href && other.id == id;

  @override
  int get hashCode => href.hashCode + id.hashCode;

  factory Addressable.fromJson(Map<String, dynamic> json) =>
      _$AddressableFromJson(json);

  Map<String, dynamic> toJson() => _$AddressableToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
