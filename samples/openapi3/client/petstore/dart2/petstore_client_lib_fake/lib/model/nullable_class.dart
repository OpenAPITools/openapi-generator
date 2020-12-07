//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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

  
  int integerProp;

  
  num numberProp;

  
  bool booleanProp;

  
  String stringProp;

  
  DateTime dateProp;

  
  DateTime datetimeProp;

  
  List<Object> arrayNullableProp;

  
  List<Object> arrayAndItemsNullableProp;

  
  List<Object> arrayItemsNullable;

  
  Map<String, Object> objectNullableProp;

  
  Map<String, Object> objectAndItemsNullableProp;

  
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
  String toString() => 'NullableClass[integerProp=$integerProp, numberProp=$numberProp, booleanProp=$booleanProp, stringProp=$stringProp, dateProp=$dateProp, datetimeProp=$datetimeProp, arrayNullableProp=$arrayNullableProp, arrayAndItemsNullableProp=$arrayAndItemsNullableProp, arrayItemsNullable=$arrayItemsNullable, objectNullableProp=$objectNullableProp, objectAndItemsNullableProp=$objectAndItemsNullableProp, objectItemsNullable=$objectItemsNullable]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (integerProp != null) {
      json['integer_prop'] = integerProp;
    }
    if (numberProp != null) {
      json['number_prop'] = numberProp;
    }
    if (booleanProp != null) {
      json['boolean_prop'] = booleanProp;
    }
    if (stringProp != null) {
      json['string_prop'] = stringProp;
    }
    if (dateProp != null) {
      json['date_prop'] = _dateFormatter.format(dateProp.toUtc());
    }
    if (datetimeProp != null) {
      json['datetime_prop'] = datetimeProp.toUtc().toIso8601String();
    }
    if (arrayNullableProp != null) {
      json['array_nullable_prop'] = arrayNullableProp;
    }
    if (arrayAndItemsNullableProp != null) {
      json['array_and_items_nullable_prop'] = arrayAndItemsNullableProp;
    }
    if (arrayItemsNullable != null) {
      json['array_items_nullable'] = arrayItemsNullable;
    }
    if (objectNullableProp != null) {
      json['object_nullable_prop'] = objectNullableProp;
    }
    if (objectAndItemsNullableProp != null) {
      json['object_and_items_nullable_prop'] = objectAndItemsNullableProp;
    }
    if (objectItemsNullable != null) {
      json['object_items_nullable'] = objectItemsNullable;
    }
    return json;
  }

  /// Returns a new [NullableClass] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static NullableClass fromJson(Map<String, dynamic> json) => json == null
    ? null
    : NullableClass(
        integerProp: json['integer_prop'],
        numberProp: json['number_prop'] == null ?
          null :
          json['number_prop'].toDouble(),
        booleanProp: json['boolean_prop'],
        stringProp: json['string_prop'],
        dateProp: json['date_prop'] == null
          ? null
          : DateTime.parse(json['date_prop']),
        datetimeProp: json['datetime_prop'] == null
          ? null
          : DateTime.parse(json['datetime_prop']),
        arrayNullableProp: Object.listFromJson(json['array_nullable_prop']),
        arrayAndItemsNullableProp: Object.listFromJson(json['array_and_items_nullable_prop']),
        arrayItemsNullable: Object.listFromJson(json['array_items_nullable']),
        objectNullableProp: json['object_nullable_prop'] == null
          ? null
          : Object.mapFromJson(json['object_nullable_prop']),
        objectAndItemsNullableProp: json['object_and_items_nullable_prop'] == null
          ? null
          : Object.mapFromJson(json['object_and_items_nullable_prop']),
        objectItemsNullable: json['object_items_nullable'] == null
          ? null
          : Object.mapFromJson(json['object_items_nullable']),
    );

  static List<NullableClass> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <NullableClass>[]
      : json.map((v) => NullableClass.fromJson(v)).toList(growable: true == growable);

  static Map<String, NullableClass> mapFromJson(Map<String, dynamic> json) {
    final map = <String, NullableClass>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = NullableClass.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of NullableClass-objects as value to a dart map
  static Map<String, List<NullableClass>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<NullableClass>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = NullableClass.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

