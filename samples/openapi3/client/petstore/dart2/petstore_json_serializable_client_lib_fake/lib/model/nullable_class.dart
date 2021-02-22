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
class NullableClass {
  /// Returns a new [NullableClass] instance.
  NullableClass({
    this.integerProp,
    this.numberProp,
    this.booleanProp,
    this.stringProp,
    this.dateProp,
    this.datetimeProp,
    this.arrayNullableProp,
    this.arrayAndItemsNullableProp,
    this.arrayItemsNullable = const [],
    this.objectNullableProp,
    this.objectAndItemsNullableProp,
    this.objectItemsNullable = const {},
  });


  @JsonKey(
    nullable: false,
    name: r'integerProp',
    required: false,
  )
  int integerProp;

  @JsonKey(
    nullable: false,
    name: r'numberProp',
    required: false,
  )
  num numberProp;

  @JsonKey(
    nullable: false,
    name: r'booleanProp',
    required: false,
  )
  bool booleanProp;

  @JsonKey(
    nullable: false,
    name: r'stringProp',
    required: false,
  )
  String stringProp;

  @JsonKey(
    nullable: false,
    name: r'dateProp',
    required: false,
  )
  DateTime dateProp;

  @JsonKey(
    nullable: false,
    name: r'datetimeProp',
    required: false,
  )
  DateTime datetimeProp;

  @JsonKey(
    nullable: false,
    name: r'arrayNullableProp',
    required: false,
  )
  List<Object> arrayNullableProp;

  @JsonKey(
    nullable: false,
    name: r'arrayAndItemsNullableProp',
    required: false,
  )
  List<Object> arrayAndItemsNullableProp;

  @JsonKey(
    nullable: false,
    name: r'arrayItemsNullable',
    required: false,
  )
  List<Object> arrayItemsNullable;

  @JsonKey(
    nullable: false,
    name: r'objectNullableProp',
    required: false,
  )
  Map<String, Object> objectNullableProp;

  @JsonKey(
    nullable: false,
    name: r'objectAndItemsNullableProp',
    required: false,
  )
  Map<String, Object> objectAndItemsNullableProp;

  @JsonKey(
    nullable: false,
    name: r'objectItemsNullable',
    required: false,
  )
  Map<String, Object> objectItemsNullable;


  @override
  bool operator ==(Object other) => identical(this, other) || other is NullableClass &&
     other.integerProp == integerProp &&
     other.numberProp == numberProp &&
     other.booleanProp == booleanProp &&
     other.stringProp == stringProp &&
     other.dateProp == dateProp &&
     other.datetimeProp == datetimeProp &&
     other.arrayNullableProp == arrayNullableProp &&
     other.arrayAndItemsNullableProp == arrayAndItemsNullableProp &&
     other.arrayItemsNullable == arrayItemsNullable &&
     other.objectNullableProp == objectNullableProp &&
     other.objectAndItemsNullableProp == objectAndItemsNullableProp &&
     other.objectItemsNullable == objectItemsNullable;

  @override
  int get hashCode =>
    (integerProp == null ? 0 : integerProp.hashCode) +
    (numberProp == null ? 0 : numberProp.hashCode) +
    (booleanProp == null ? 0 : booleanProp.hashCode) +
    (stringProp == null ? 0 : stringProp.hashCode) +
    (dateProp == null ? 0 : dateProp.hashCode) +
    (datetimeProp == null ? 0 : datetimeProp.hashCode) +
    (arrayNullableProp == null ? 0 : arrayNullableProp.hashCode) +
    (arrayAndItemsNullableProp == null ? 0 : arrayAndItemsNullableProp.hashCode) +
    (arrayItemsNullable == null ? 0 : arrayItemsNullable.hashCode) +
    (objectNullableProp == null ? 0 : objectNullableProp.hashCode) +
    (objectAndItemsNullableProp == null ? 0 : objectAndItemsNullableProp.hashCode) +
    (objectItemsNullable == null ? 0 : objectItemsNullable.hashCode);


  factory NullableClass.fromJson(Map<String, dynamic> json) => _$NullableClassFromJson(json);

  Map<String, dynamic> toJson() => _$NullableClassToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

