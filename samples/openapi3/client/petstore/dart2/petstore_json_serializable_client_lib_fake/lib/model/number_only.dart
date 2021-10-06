//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class NumberOnly {
  /// Returns a new [NumberOnly] instance.
  NumberOnly({
    this.justNumber,
  });


  @JsonKey(
    name: r'JustNumber',
    required: false,
  )
  num? justNumber;

  @override
  bool operator ==(Object other) => identical(this, other) || other is NumberOnly &&
     other.justNumber == justNumber;

  @override
  int get hashCode =>
    justNumber.hashCode;

  factory NumberOnly.fromJson(Map<String, dynamic> json) => _$NumberOnlyFromJson(json);

  Map<String, dynamic> toJson() => _$NumberOnlyToJson(this);

  @override
  String toString() => toJson().toString();
}

