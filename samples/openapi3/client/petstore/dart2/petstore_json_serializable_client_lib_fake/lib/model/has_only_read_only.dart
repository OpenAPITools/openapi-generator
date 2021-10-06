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
class HasOnlyReadOnly {
  /// Returns a new [HasOnlyReadOnly] instance.
  HasOnlyReadOnly({
    this.bar,
    this.foo,
  });


  @JsonKey(
    name: r'bar',
    required: false,
  )
  String? bar;

  @JsonKey(
    name: r'foo',
    required: false,
  )
  String? foo;

  @override
  bool operator ==(Object other) => identical(this, other) || other is HasOnlyReadOnly &&
     other.bar == bar &&
     other.foo == foo;

  @override
  int get hashCode =>
    bar.hashCode +
    foo.hashCode;

  factory HasOnlyReadOnly.fromJson(Map<String, dynamic> json) => _$HasOnlyReadOnlyFromJson(json);

  Map<String, dynamic> toJson() => _$HasOnlyReadOnlyToJson(this);

  @override
  String toString() => toJson().toString();
}

