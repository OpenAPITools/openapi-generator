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
    name: r'integer_prop',
    required: false,
  )
  int? integerProp;

  @JsonKey(
    name: r'number_prop',
    required: false,
  )
  num? numberProp;

  @JsonKey(
    name: r'boolean_prop',
    required: false,
  )
  bool? booleanProp;

  @JsonKey(
    name: r'string_prop',
    required: false,
  )
  String? stringProp;

  @JsonKey(
    name: r'date_prop',
    required: false,
  )
  DateTime? dateProp;

  @JsonKey(
    name: r'datetime_prop',
    required: false,
  )
  DateTime? datetimeProp;

  @JsonKey(
    defaultValue: const [],
    name: r'array_nullable_prop',
    required: false,
  )
  List<Map<String, dynamic>>? arrayNullableProp;

  @JsonKey(
    defaultValue: const [],
    name: r'array_and_items_nullable_prop',
    required: false,
  )
  List<Map<String, dynamic>>? arrayAndItemsNullableProp;

  @JsonKey(
    defaultValue: const [],
    name: r'array_items_nullable',
    required: false,
  )
  List<Map<String, dynamic>>? arrayItemsNullable;

  @JsonKey(
    defaultValue: const {},
    name: r'object_nullable_prop',
    required: false,
  )
  Map<String, dynamic>? objectNullableProp;

  @JsonKey(
    defaultValue: const {},
    name: r'object_and_items_nullable_prop',
    required: false,
  )
  Map<String, dynamic>? objectAndItemsNullableProp;

  @JsonKey(
    defaultValue: const {},
    name: r'object_items_nullable',
    required: false,
  )
  Map<String, dynamic>? objectItemsNullable;

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
    integerProp.hashCode +
    numberProp.hashCode +
    booleanProp.hashCode +
    stringProp.hashCode +
    dateProp.hashCode +
    datetimeProp.hashCode +
    arrayNullableProp.hashCode +
    arrayAndItemsNullableProp.hashCode +
    arrayItemsNullable.hashCode +
    objectNullableProp.hashCode +
    objectAndItemsNullableProp.hashCode +
    objectItemsNullable.hashCode;

  factory NullableClass.fromJson(Map<String, dynamic> json) => _$NullableClassFromJson(json);

  Map<String, dynamic> toJson() => _$NullableClassToJson(this);

  @override
  String toString() => toJson().toString();
}

