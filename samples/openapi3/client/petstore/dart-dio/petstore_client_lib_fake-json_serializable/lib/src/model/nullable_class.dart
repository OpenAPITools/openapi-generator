//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'nullable_class.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
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

     this.arrayItemsNullable,

     this.objectNullableProp,

     this.objectAndItemsNullableProp,

     this.objectItemsNullable,
  });

  @JsonKey(
    
    name: r'integer_prop',
    required: false,
    includeIfNull: false
  )


  final int? integerProp;



  @JsonKey(
    
    name: r'number_prop',
    required: false,
    includeIfNull: false
  )


  final num? numberProp;



  @JsonKey(
    
    name: r'boolean_prop',
    required: false,
    includeIfNull: false
  )


  final bool? booleanProp;



  @JsonKey(
    
    name: r'string_prop',
    required: false,
    includeIfNull: false
  )


  final String? stringProp;



  @JsonKey(
    
    name: r'date_prop',
    required: false,
    includeIfNull: false
  )


  final DateTime? dateProp;



  @JsonKey(
    
    name: r'datetime_prop',
    required: false,
    includeIfNull: false
  )


  final DateTime? datetimeProp;



  @JsonKey(
    
    name: r'array_nullable_prop',
    required: false,
    includeIfNull: false
  )


  final List<Object>? arrayNullableProp;



  @JsonKey(
    
    name: r'array_and_items_nullable_prop',
    required: false,
    includeIfNull: false
  )


  final List<Object>? arrayAndItemsNullableProp;



  @JsonKey(
    
    name: r'array_items_nullable',
    required: false,
    includeIfNull: false
  )


  final List<Object>? arrayItemsNullable;



  @JsonKey(
    
    name: r'object_nullable_prop',
    required: false,
    includeIfNull: false
  )


  final Map<String, Object>? objectNullableProp;



  @JsonKey(
    
    name: r'object_and_items_nullable_prop',
    required: false,
    includeIfNull: false
  )


  final Map<String, Object>? objectAndItemsNullableProp;



  @JsonKey(
    
    name: r'object_items_nullable',
    required: false,
    includeIfNull: false
  )


  final Map<String, Object>? objectItemsNullable;



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
    arrayItemsNullable.hashCode +
    (objectNullableProp == null ? 0 : objectNullableProp.hashCode) +
    (objectAndItemsNullableProp == null ? 0 : objectAndItemsNullableProp.hashCode) +
    objectItemsNullable.hashCode;

  factory NullableClass.fromJson(Map<String, dynamic> json) => _$NullableClassFromJson(json);

  Map<String, dynamic> toJson() => _$NullableClassToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

