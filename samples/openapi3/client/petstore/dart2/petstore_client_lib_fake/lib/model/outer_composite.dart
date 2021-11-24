//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class OuterComposite {
  /// Returns a new [OuterComposite] instance.
  OuterComposite({
    this.myNumber,
    this.myString,
    this.myBoolean,
  });

  num myNumber;

  String myString;

  bool myBoolean;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterComposite &&
     other.myNumber == myNumber &&
     other.myString == myString &&
     other.myBoolean == myBoolean;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (myNumber == null ? 0 : myNumber.hashCode) +
    (myString == null ? 0 : myString.hashCode) +
    (myBoolean == null ? 0 : myBoolean.hashCode);

  @override
  String toString() => 'OuterComposite[myNumber=$myNumber, myString=$myString, myBoolean=$myBoolean]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (myNumber != null) {
      json[r'my_number'] = myNumber;
    }
    if (myString != null) {
      json[r'my_string'] = myString;
    }
    if (myBoolean != null) {
      json[r'my_boolean'] = myBoolean;
    }
    return json;
  }

  /// Returns a new [OuterComposite] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static OuterComposite fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return OuterComposite(
        myNumber: json[r'my_number'] == null
          ? null
          : num.parse(json[r'my_number'].toString()),
        myString: mapValueOfType<String>(json, r'my_string'),
        myBoolean: mapValueOfType<bool>(json, r'my_boolean'),
      );
    }
    return null;
  }

  static List<OuterComposite> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(OuterComposite.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <OuterComposite>[];

  static Map<String, OuterComposite> mapFromJson(dynamic json) {
    final map = <String, OuterComposite>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = OuterComposite.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of OuterComposite-objects as value to a dart map
  static Map<String, List<OuterComposite>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<OuterComposite>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = OuterComposite.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

