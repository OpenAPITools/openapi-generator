//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

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


  int? integerProp;

  num? numberProp;

  bool? booleanProp;

  String? stringProp;

  DateTime? dateProp;

  DateTime? datetimeProp;

  List<Map<String, dynamic>>? arrayNullableProp;

  List<Map<String, dynamic>>? arrayAndItemsNullableProp;

  List<Map<String, dynamic>>? arrayItemsNullable;

  Map<String, dynamic>? objectNullableProp;

  Map<String, dynamic>? objectAndItemsNullableProp;

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

  @override
  String toString() => 'NullableClass[integerProp=$integerProp, numberProp=$numberProp, booleanProp=$booleanProp, stringProp=$stringProp, dateProp=$dateProp, datetimeProp=$datetimeProp, arrayNullableProp=$arrayNullableProp, arrayAndItemsNullableProp=$arrayAndItemsNullableProp, arrayItemsNullable=$arrayItemsNullable, objectNullableProp=$objectNullableProp, objectAndItemsNullableProp=$objectAndItemsNullableProp, objectItemsNullable=$objectItemsNullable]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (integerProp != null) {
      json[r'integer_prop'] = integerProp;
    }
    if (numberProp != null) {
      json[r'number_prop'] = numberProp;
    }
    if (booleanProp != null) {
      json[r'boolean_prop'] = booleanProp;
    }
    if (stringProp != null) {
      json[r'string_prop'] = stringProp;
    }
    if (dateProp != null) {
      json[r'date_prop'] = _dateFormatter.format(dateProp!.toUtc());
    }
    if (datetimeProp != null) {
      json[r'datetime_prop'] = datetimeProp!.toUtc().toIso8601String();
    }
    if (arrayNullableProp != null) {
      json[r'array_nullable_prop'] = arrayNullableProp;
    }
    if (arrayAndItemsNullableProp != null) {
      json[r'array_and_items_nullable_prop'] = arrayAndItemsNullableProp;
    }
    if (arrayItemsNullable != null) {
      json[r'array_items_nullable'] = arrayItemsNullable;
    }
    if (objectNullableProp != null) {
      json[r'object_nullable_prop'] = objectNullableProp;
    }
    if (objectAndItemsNullableProp != null) {
      json[r'object_and_items_nullable_prop'] = objectAndItemsNullableProp;
    }
    if (objectItemsNullable != null) {
      json[r'object_items_nullable'] = objectItemsNullable;
    }
    return json;
  }

  /// Returns a new [NullableClass] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static NullableClass fromJson(Map<String, dynamic> json) => NullableClass(
        integerProp: json[r'integer_prop'] as int,
        numberProp: json[r'number_prop'] as num,
        booleanProp: json[r'boolean_prop'] as bool,
        stringProp: json[r'string_prop'] as String,
        dateProp: mapDateTime(json, r'date_prop', ''),
        datetimeProp: mapDateTime(json, r'datetime_prop', ''),
        arrayNullableProp: (json[r'array_nullable_prop'] as List).cast<Map<String, dynamic>>(),
        arrayAndItemsNullableProp: (json[r'array_and_items_nullable_prop'] as List).cast<Map<String, dynamic>>(),
        arrayItemsNullable: (json[r'array_items_nullable'] as List).cast<Map<String, dynamic>>(),
        objectNullableProp: json[r'object_nullable_prop'] as Map<String, dynamic>,
        objectAndItemsNullableProp: json[r'object_and_items_nullable_prop'] as Map<String, dynamic>,
        objectItemsNullable: json[r'object_items_nullable'] as Map<String, dynamic>,
    );

  static List<NullableClass> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<NullableClass>((i) => NullableClass.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <NullableClass>[];

  static Map<String, NullableClass> mapFromJson(dynamic json) {
    final map = <String, NullableClass>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = NullableClass.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of NullableClass-objects as value to a dart map
  static Map<String, List<NullableClass>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<NullableClass>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = NullableClass.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

