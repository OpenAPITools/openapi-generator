//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class SingleRefType {
  /// Instantiate a new enum with the provided [value].
  const SingleRefType._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const admin = SingleRefType._(r'admin');
  static const user = SingleRefType._(r'user');

  /// List of all possible values in this [enum][SingleRefType].
  static const values = <SingleRefType>[
    admin,
    user,
  ];

  static SingleRefType? fromJson(dynamic value) => SingleRefTypeTypeTransformer().decode(value);

  static List<SingleRefType> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <SingleRefType>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = SingleRefType.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [SingleRefType] to String,
/// and [decode] dynamic data back to [SingleRefType].
class SingleRefTypeTypeTransformer {
  factory SingleRefTypeTypeTransformer() => _instance ??= const SingleRefTypeTypeTransformer._();

  const SingleRefTypeTypeTransformer._();

  String encode(SingleRefType data) => data.value;

  /// Decodes a [dynamic value][data] to a SingleRefType.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  SingleRefType? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data) {
        case r'admin': return SingleRefType.admin;
        case r'user': return SingleRefType.user;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [SingleRefTypeTypeTransformer] instance.
  static SingleRefTypeTypeTransformer? _instance;
}

