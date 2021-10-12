//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

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


  num? myNumber;

  String? myString;

  bool? myBoolean;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterComposite &&
     other.myNumber == myNumber &&
     other.myString == myString &&
     other.myBoolean == myBoolean;

  @override
  int get hashCode =>
    myNumber.hashCode +
    myString.hashCode +
    myBoolean.hashCode;

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
  static OuterComposite fromJson(Map<String, dynamic> json) => OuterComposite(
        myNumber: json[r'my_number'] as num,
        myString: json[r'my_string'] as String,
        myBoolean: json[r'my_boolean'] as bool,
    );

  static List<OuterComposite> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<OuterComposite>((i) => OuterComposite.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <OuterComposite>[];

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
  static Map<String, List<OuterComposite>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<OuterComposite>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = OuterComposite.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

