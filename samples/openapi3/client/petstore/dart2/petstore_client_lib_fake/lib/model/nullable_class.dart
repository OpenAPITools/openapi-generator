//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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
  // ignore: unnecessary_parenthesis
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
      json[r'date_prop'] = _dateFormatter.format(dateProp.toUtc());
    }
    if (datetimeProp != null) {
      json[r'datetime_prop'] = datetimeProp.toUtc().toIso8601String();
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
  static NullableClass fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return NullableClass(
        integerProp: mapValueOfType<int>(json, r'integer_prop'),
        numberProp: json[r'number_prop'] == null
          ? null
          : num.parse(json[r'number_prop'].toString()),
        booleanProp: mapValueOfType<bool>(json, r'boolean_prop'),
        stringProp: mapValueOfType<String>(json, r'string_prop'),
        dateProp: mapDateTime(json, r'date_prop', ''),
        datetimeProp: mapDateTime(json, r'datetime_prop', ''),
        arrayNullableProp: Object.listFromJson(json[r'array_nullable_prop']),
        arrayAndItemsNullableProp: Object.listFromJson(json[r'array_and_items_nullable_prop']),
        arrayItemsNullable: Object.listFromJson(json[r'array_items_nullable']),
        objectNullableProp: mapValueOfType<Map<String, Object>>(json, r'object_nullable_prop'),
        objectAndItemsNullableProp: mapValueOfType<Map<String, Object>>(json, r'object_and_items_nullable_prop'),
        objectItemsNullable: mapValueOfType<Map<String, Object>>(json, r'object_items_nullable'),
      );
    }
    return null;
  }

  static List<NullableClass> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(NullableClass.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <NullableClass>[];

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
  static Map<String, List<NullableClass>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<NullableClass>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = NullableClass.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

