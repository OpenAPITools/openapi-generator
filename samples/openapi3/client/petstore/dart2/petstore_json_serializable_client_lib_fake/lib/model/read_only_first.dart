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
class ReadOnlyFirst {
  /// Returns a new [ReadOnlyFirst] instance.
  ReadOnlyFirst({
    this.bar,
    this.baz,
  });

  @JsonKey(
    nullable: false,
    name: r'bar',
    required: false,
  )
  String bar;

  @JsonKey(
    nullable: false,
    name: r'baz',
    required: false,
  )
  String baz;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ReadOnlyFirst &&
     other.bar == bar &&
     other.baz == baz;

  @override
  int get hashCode =>
    (bar == null ? 0 : bar.hashCode) +
    (baz == null ? 0 : baz.hashCode);

  factory ReadOnlyFirst.fromJson(Map<String, dynamic> json) => _$ReadOnlyFirstFromJson(json);

  Map<String, dynamic> toJson() => _$ReadOnlyFirstToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

