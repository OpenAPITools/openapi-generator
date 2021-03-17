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
class OuterComposite {
  /// Returns a new [OuterComposite] instance.
  OuterComposite({
    this.myNumber,
    this.myString,
    this.myBoolean,
  });

  @JsonKey(
    nullable: false,
    name: r'my_number',
    required: false,
  )
  num myNumber;

  @JsonKey(
    nullable: false,
    name: r'my_string',
    required: false,
  )
  String myString;

  @JsonKey(
    nullable: false,
    name: r'my_boolean',
    required: false,
  )
  bool myBoolean;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterComposite &&
     other.myNumber == myNumber &&
     other.myString == myString &&
     other.myBoolean == myBoolean;

  @override
  int get hashCode =>
    (myNumber == null ? 0 : myNumber.hashCode) +
    (myString == null ? 0 : myString.hashCode) +
    (myBoolean == null ? 0 : myBoolean.hashCode);

  factory OuterComposite.fromJson(Map<String, dynamic> json) => _$OuterCompositeFromJson(json);

  Map<String, dynamic> toJson() => _$OuterCompositeToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

