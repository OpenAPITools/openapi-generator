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
class InlineResponseDefault {
  /// Returns a new [InlineResponseDefault] instance.
  InlineResponseDefault({
    this.string,
  });

  @JsonKey(
    nullable: false,
    name: r'string',
    required: false,
  )
  Foo string;

  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineResponseDefault &&
     other.string == string;

  @override
  int get hashCode =>
    (string == null ? 0 : string.hashCode);

  factory InlineResponseDefault.fromJson(Map<String, dynamic> json) => _$InlineResponseDefaultFromJson(json);

  Map<String, dynamic> toJson() => _$InlineResponseDefaultToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

