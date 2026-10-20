//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


enum OuterEnumDefaultValue {
  placed._(r'placed'),
  approved._(r'approved'),
  delivered._(r'delivered'),
  ;

  /// Instantiate a new enum with the provided value.
  const OuterEnumDefaultValue._(this._value);

  /// The underlying value of this enum member.
  final String _value;

  @override
  String toString() => _value;

  /// Encodes this enum as a value suitable for JSON.
  String toJson() => _value;

  /// Returns the instance of [OuterEnumDefaultValue] that was successfully decoded
  /// from the passed [value] on success, null otherwise.
  static OuterEnumDefaultValue? fromJson(dynamic value) => OuterEnumDefaultValueTypeTransformer().decode(value);

  /// Returns a [List] containing instances of [OuterEnumDefaultValue]
  /// that were successfully decoded from the passed [JSON][json].
  static List<OuterEnumDefaultValue> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <OuterEnumDefaultValue>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = OuterEnumDefaultValue.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [OuterEnumDefaultValue] to String,
/// and [decode] dynamic data back to [OuterEnumDefaultValue].
class OuterEnumDefaultValueTypeTransformer {
  factory OuterEnumDefaultValueTypeTransformer() => _instance ??= const OuterEnumDefaultValueTypeTransformer._();

  const OuterEnumDefaultValueTypeTransformer._();

  /// Encodes this enum as a value suitable for JSON.
  String encode(OuterEnumDefaultValue data) => data._value;

  /// Returns the instance of [OuterEnumDefaultValue] that was successfully decoded
  /// from the passed [data] value on success, null otherwise.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  OuterEnumDefaultValue? decode(dynamic data, {bool allowNull = true}) {
    if (data is OuterEnumDefaultValue) {
      return data;
    }
    if (data != null) {
      switch (data) {
        case r'placed': return OuterEnumDefaultValue.placed;
        case r'approved': return OuterEnumDefaultValue.approved;
        case r'delivered': return OuterEnumDefaultValue.delivered;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// The singleton instance of this transformer.
  static OuterEnumDefaultValueTypeTransformer? _instance;
}

