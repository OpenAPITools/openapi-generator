//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class OuterComposite {
  /// Returns a new [OuterComposite] instance.
  OuterComposite({
    this.myNumber,
    this.myString,
    this.myBoolean,
  });

  /// Returns a new [OuterComposite] instance and optionally import its values from
  /// [json] if it's non-null.
  OuterComposite.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      myNumber = json['my_number'] == null ?
        null :
        json['my_number'].toDouble();
      myString = json['my_string'];
      myBoolean = json['my_boolean'];
    }
  }

  
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
    myNumber.hashCode +
    myString.hashCode +
    myBoolean.hashCode;

  @override
  String toString() => 'OuterComposite[myNumber=$myNumber, myString=$myString, myBoolean=$myBoolean]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (myNumber != null) {
      json['my_number'] = myNumber;
    }
    if (myString != null) {
      json['my_string'] = myString;
    }
    if (myBoolean != null) {
      json['my_boolean'] = myBoolean;
    }
    return json;
  }

  static List<OuterComposite> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <OuterComposite>[]
      : json.map((v) => OuterComposite.fromJson(v)).toList(growable: true == growable);

  static Map<String, OuterComposite> mapFromJson(Map<String, dynamic> json) {
    final map = <String, OuterComposite>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = OuterComposite.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of OuterComposite-objects as value to a dart map
  static Map<String, List<OuterComposite>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<OuterComposite>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = OuterComposite.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

