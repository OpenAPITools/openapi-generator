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
class OuterComposite {
  /// Returns a new [OuterComposite] instance.
  OuterComposite({
    this.myNumber,
    this.myString,
    this.myBoolean,
  });


  @JsonKey(
    name: r'my_number',
    required: false,
  )
  num? myNumber;

  @JsonKey(
    name: r'my_string',
    required: false,
  )
  String? myString;

  @JsonKey(
    name: r'my_boolean',
    required: false,
  )
  bool? myBoolean;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterComposite &&
     other.myNumber == myNumber &&
     other.myString == myString &&
     other.myBoolean == myBoolean;

  @override
  int get hashCode =>
    myNumber.hashCode +
    myString.hashCode +
    myBoolean.hashCode;

  factory OuterComposite.fromJson(Map<String, dynamic> json) => _$OuterCompositeFromJson(json);

  Map<String, dynamic> toJson() => _$OuterCompositeToJson(this);

  @override
  String toString() => toJson().toString();
}

