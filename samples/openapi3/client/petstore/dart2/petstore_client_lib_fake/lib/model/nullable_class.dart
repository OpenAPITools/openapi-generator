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
    this.arrayNullableProp = const [],
    this.arrayAndItemsNullableProp = const [],
    this.arrayItemsNullable = const [],
    this.objectNullableProp = const {},
    this.objectAndItemsNullableProp = const {},
    this.objectItemsNullable = const {},
  });

  int? integerProp;

  num? numberProp;

  bool? booleanProp;

  String? stringProp;

  DateTime? dateProp;

  DateTime? datetimeProp;

  List<Object>? arrayNullableProp;

  List<Object>? arrayAndItemsNullableProp;

  List<Object> arrayItemsNullable;

  Map<String, Object>? objectNullableProp;

  Map<String, Object>? objectAndItemsNullableProp;

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
    (integerProp == null ? 0 : integerProp!.hashCode) +
    (numberProp == null ? 0 : numberProp!.hashCode) +
    (booleanProp == null ? 0 : booleanProp!.hashCode) +
    (stringProp == null ? 0 : stringProp!.hashCode) +
    (dateProp == null ? 0 : dateProp!.hashCode) +
    (datetimeProp == null ? 0 : datetimeProp!.hashCode) +
    (arrayNullableProp == null ? 0 : arrayNullableProp!.hashCode) +
    (arrayAndItemsNullableProp == null ? 0 : arrayAndItemsNullableProp!.hashCode) +
    (arrayItemsNullable.hashCode) +
    (objectNullableProp == null ? 0 : objectNullableProp!.hashCode) +
    (objectAndItemsNullableProp == null ? 0 : objectAndItemsNullableProp!.hashCode) +
    (objectItemsNullable.hashCode);

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
      json[r'array_items_nullable'] = arrayItemsNullable;
    if (objectNullableProp != null) {
      json[r'object_nullable_prop'] = objectNullableProp;
    }
    if (objectAndItemsNullableProp != null) {
      json[r'object_and_items_nullable_prop'] = objectAndItemsNullableProp;
    }
      json[r'object_items_nullable'] = objectItemsNullable;
    return json;
  }

  /// Returns a new [NullableClass] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static NullableClass? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "NullableClass[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "NullableClass[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return NullableClass(
        integerProp: mapValueOfType<int>(json, r'integer_prop'),
        numberProp: json[r'number_prop'] == null
            ? null
            : num.parse(json[r'number_prop'].toString()),
        booleanProp: mapValueOfType<bool>(json, r'boolean_prop'),
        stringProp: mapValueOfType<String>(json, r'string_prop'),
        dateProp: mapDateTime(json, r'date_prop', ''),
        datetimeProp: mapDateTime(json, r'datetime_prop', ''),
        arrayNullableProp: Object.listFromJson(json[r'array_nullable_prop']) ?? const [],
        arrayAndItemsNullableProp: Object.listFromJson(json[r'array_and_items_nullable_prop']) ?? const [],
        arrayItemsNullable: Object.listFromJson(json[r'array_items_nullable']) ?? const [],
        objectNullableProp: mapValueOfType<Map<String, Object>>(json, r'object_nullable_prop') ?? const {},
        objectAndItemsNullableProp: mapValueOfType<Map<String, Object>>(json, r'object_and_items_nullable_prop') ?? const {},
        objectItemsNullable: mapValueOfType<Map<String, Object>>(json, r'object_items_nullable') ?? const {},
      );
    }
    return null;
  }

  static List<NullableClass>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <NullableClass>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = NullableClass.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, NullableClass> mapFromJson(dynamic json) {
    final map = <String, NullableClass>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = NullableClass.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of NullableClass-objects as value to a dart map
  static Map<String, List<NullableClass>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<NullableClass>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = NullableClass.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

