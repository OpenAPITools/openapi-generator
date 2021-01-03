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
    name: r'integerProp',
    
    
    
  )
  int integerProp;

  @JsonKey(
    name: r'numberProp',
    
    
    
  )
  num numberProp;

  @JsonKey(
    name: r'booleanProp',
    
    
    
  )
  bool booleanProp;

  @JsonKey(
    name: r'stringProp',
    
    
    
  )
  String stringProp;

  @JsonKey(
    name: r'dateProp',
    
    
    
  )
  DateTime dateProp;

  @JsonKey(
    name: r'datetimeProp',
    
    
    
  )
  DateTime datetimeProp;

  @JsonKey(
    name: r'arrayNullableProp',
    
    defaultValue: const [],
    
  )
  List<Object> arrayNullableProp;

  @JsonKey(
    name: r'arrayAndItemsNullableProp',
    
    defaultValue: const [],
    
  )
  List<Object> arrayAndItemsNullableProp;

  @JsonKey(
    name: r'arrayItemsNullable',
    
    defaultValue: const [],
    
  )
  List<Object> arrayItemsNullable;

  @JsonKey(
    name: r'objectNullableProp',
    
    defaultValue: const {},
    
  )
  Map<String, Object> objectNullableProp;

  @JsonKey(
    name: r'objectAndItemsNullableProp',
    
    defaultValue: const {},
    
  )
  Map<String, Object> objectAndItemsNullableProp;

  @JsonKey(
    name: r'objectItemsNullable',
    
    defaultValue: const {},
    
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

  @override
  String toString() => toJson().toString();

  factory NullableClass.fromJson(Map<String, dynamic> json) => _$NullableClassFromJson(json);
  Map<String, dynamic> toJson() => _$NullableClassToJson(this);
}

