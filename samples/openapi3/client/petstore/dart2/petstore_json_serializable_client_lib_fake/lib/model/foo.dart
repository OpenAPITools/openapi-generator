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
class Foo {
  /// Returns a new [Foo] instance.
  Foo({
    this.bar = 'bar',
  });

  @JsonKey(
    defaultValue: 'bar',
    name: r'bar',
    required: false,
  )
  String bar;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Foo &&
     other.bar == bar;

  @override
  int get hashCode =>
    (bar == null ? 0 : bar.hashCode);

  factory Foo.fromJson(Map<String, dynamic> json) => _$FooFromJson(json);

  Map<String, dynamic> toJson() => _$FooToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

