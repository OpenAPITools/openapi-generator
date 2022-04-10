//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class UserType {
  /// Instantiate a new enum with the provided [value].
  const UserType._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const admin = UserType._(r'admin');
  static const user = UserType._(r'user');

  /// List of all possible values in this [enum][UserType].
  static const values = <UserType>[
    admin,
    user,
  ];

  static UserType? fromJson(dynamic value) => UserTypeTypeTransformer().decode(value);

  static List<UserType>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <UserType>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = UserType.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [UserType] to String,
/// and [decode] dynamic data back to [UserType].
class UserTypeTypeTransformer {
  factory UserTypeTypeTransformer() => _instance ??= const UserTypeTypeTransformer._();

  const UserTypeTypeTransformer._();

  String encode(UserType data) => data.value;

  /// Decodes a [dynamic value][data] to a UserType.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  UserType? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data.toString()) {
        case r'admin': return UserType.admin;
        case r'user': return UserType.user;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [UserTypeTypeTransformer] instance.
  static UserTypeTypeTransformer? _instance;
}

