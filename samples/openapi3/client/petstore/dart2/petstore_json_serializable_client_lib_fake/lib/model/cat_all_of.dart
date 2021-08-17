//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class CatAllOf {
  /// Returns a new [CatAllOf] instance.
  CatAllOf({
    this.declawed,
  });

  @JsonKey(
    nullable: false,
    name: r'declawed',
    required: false,
  )
  bool declawed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is CatAllOf &&
     other.declawed == declawed;

  @override
  int get hashCode =>
    (declawed == null ? 0 : declawed.hashCode);

  factory CatAllOf.fromJson(Map<String, dynamic> json) => _$CatAllOfFromJson(json);

  Map<String, dynamic> toJson() => _$CatAllOfToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

