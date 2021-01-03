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
  includeIfNull: false,
  disallowUnrecognizedKeys: true,
)
class ReadOnlyFirst {
  /// Returns a new [ReadOnlyFirst] instance.
  ReadOnlyFirst({
    this.bar,
    this.baz,
  });

  @JsonKey(
    name: r'bar',
    
    
    
  )
  String bar;

  @JsonKey(
    name: r'baz',
    
    
    
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

  @override
  String toString() => toJson().toString();

  factory ReadOnlyFirst.fromJson(Map<String, dynamic> json) => _$ReadOnlyFirstFromJson(json);
  Map<String, dynamic> toJson() => _$ReadOnlyFirstToJson(this);
}

