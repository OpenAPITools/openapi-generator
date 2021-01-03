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
class OuterComposite {
  /// Returns a new [OuterComposite] instance.
  OuterComposite({
    this.myNumber,
    this.myString,
    this.myBoolean,
  });

  @JsonKey(
    name: r'myNumber',
    
    
    
  )
  num myNumber;

  @JsonKey(
    name: r'myString',
    
    
    
  )
  String myString;

  @JsonKey(
    name: r'myBoolean',
    
    
    
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

  @override
  String toString() => toJson().toString();

  factory OuterComposite.fromJson(Map<String, dynamic> json) => _$OuterCompositeFromJson(json);
  Map<String, dynamic> toJson() => _$OuterCompositeToJson(this);
}

