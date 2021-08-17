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
    nullable: true,
    name: r'integer_prop',
    required: false,
  )
  int integerProp;

  @JsonKey(
    nullable: true,
    name: r'number_prop',
    required: false,
  )
  num numberProp;

  @JsonKey(
    nullable: true,
    name: r'boolean_prop',
    required: false,
  )
  bool booleanProp;

  @JsonKey(
    nullable: true,
    name: r'string_prop',
    required: false,
  )
  String stringProp;

  @JsonKey(
    nullable: true,
    name: r'date_prop',
    required: false,
  )
  DateTime dateProp;

  @JsonKey(
    nullable: true,
    name: r'datetime_prop',
    required: false,
  )
  DateTime datetimeProp;

  @JsonKey(
    defaultValue: const [],
    name: r'array_nullable_prop',
    required: false,
  )
  List<Object> arrayNullableProp;

  @JsonKey(
    defaultValue: const [],
    name: r'array_and_items_nullable_prop',
    required: false,
  )
  List<Object> arrayAndItemsNullableProp;

  @JsonKey(
    defaultValue: const [],
    name: r'array_items_nullable',
    required: false,
  )
  List<Object> arrayItemsNullable;

  @JsonKey(
    defaultValue: const {},
    name: r'object_nullable_prop',
    required: false,
  )
  Map<String, Object> objectNullableProp;

  @JsonKey(
    defaultValue: const {},
    name: r'object_and_items_nullable_prop',
    required: false,
  )
  Map<String, Object> objectAndItemsNullableProp;

  @JsonKey(
    defaultValue: const {},
    name: r'object_items_nullable',
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

