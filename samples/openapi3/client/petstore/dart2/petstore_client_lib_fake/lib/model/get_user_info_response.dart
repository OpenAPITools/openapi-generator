//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class GetUserInfoResponse {
  /// Returns a new [GetUserInfoResponse] instance.
  GetUserInfoResponse({
    this.email,
    this.scopes = const [],
  });

  /// Email address of user, if available
  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? email;

  /// Auth-Scopes of the user, if available
  List<GetUserInfoResponseScopesEnum> scopes;

  @override
  bool operator ==(Object other) => identical(this, other) || other is GetUserInfoResponse &&
    other.email == email &&
    _deepEquality.equals(other.scopes, scopes);

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (email == null ? 0 : email!.hashCode) +
    (scopes.hashCode);

  @override
  String toString() => 'GetUserInfoResponse[email=$email, scopes=$scopes]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.email != null) {
      json[r'email'] = this.email;
    } else {
      json[r'email'] = null;
    }
      json[r'scopes'] = this.scopes;
    return json;
  }

  /// Returns a new [GetUserInfoResponse] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static GetUserInfoResponse? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        return true;
      }());

      return GetUserInfoResponse(
        email: mapValueOfType<String>(json, r'email'),
        scopes: GetUserInfoResponseScopesEnum.listFromJson(json[r'scopes']),
      );
    }
    return null;
  }

  static List<GetUserInfoResponse> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <GetUserInfoResponse>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = GetUserInfoResponse.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, GetUserInfoResponse> mapFromJson(dynamic json) {
    final map = <String, GetUserInfoResponse>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = GetUserInfoResponse.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of GetUserInfoResponse-objects as value to a dart map
  static Map<String, List<GetUserInfoResponse>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<GetUserInfoResponse>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = GetUserInfoResponse.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}


enum GetUserInfoResponseScopesEnum {
  app._(r'app'),
  cms._(r'cms'),
  ;

  /// Instantiate a new enum with the provided value.
  const GetUserInfoResponseScopesEnum._(this._value);

  /// The underlying value of this enum member.
  final String _value;

  @override
  String toString() => _value;

  /// Encodes this enum as a value suitable for JSON.
  String toJson() => _value;

  /// Returns the instance of [GetUserInfoResponseScopesEnum] that was successfully decoded
  /// from the passed [value] on success, null otherwise.
  static GetUserInfoResponseScopesEnum? fromJson(dynamic value) => GetUserInfoResponseScopesEnumTypeTransformer().decode(value);

  /// Returns a [List] containing instances of [GetUserInfoResponseScopesEnum]
  /// that were successfully decoded from the passed [JSON][json].
  static List<GetUserInfoResponseScopesEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <GetUserInfoResponseScopesEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = GetUserInfoResponseScopesEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [GetUserInfoResponseScopesEnum] to String,
/// and [decode] dynamic data back to [GetUserInfoResponseScopesEnum].
class GetUserInfoResponseScopesEnumTypeTransformer {
  factory GetUserInfoResponseScopesEnumTypeTransformer() => _instance ??= const GetUserInfoResponseScopesEnumTypeTransformer._();

  const GetUserInfoResponseScopesEnumTypeTransformer._();

  String encode(GetUserInfoResponseScopesEnum data) => data._value;

  /// Returns the instance of [GetUserInfoResponseScopesEnum] that was successfully decoded
  /// from the passed [data] value on success, null otherwise.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  GetUserInfoResponseScopesEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data is GetUserInfoResponseScopesEnum) {
      return data;
    }
    if (data != null) {
      switch (data) {
        case r'app': return GetUserInfoResponseScopesEnum.app;
        case r'cms': return GetUserInfoResponseScopesEnum.cms;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// The singleton instance of this transformer.
  static GetUserInfoResponseScopesEnumTypeTransformer? _instance;
}


