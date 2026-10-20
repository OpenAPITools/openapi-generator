//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


enum OuterEnumIntegerDefaultValue {
  number0._(0),
  number1._(1),
  number2._(2),
  ;

  /// Instantiate a new enum with the provided value.
  const OuterEnumIntegerDefaultValue._(this._value);

  /// The underlying value of this enum member.
  final int _value;

  @override
  String toString() => _value.toString();

  /// Encodes this enum as a value suitable for JSON.
  int toJson() => _value;

  /// Returns the instance of [OuterEnumIntegerDefaultValue] that was successfully decoded
  /// from the passed [value] on success, null otherwise.
  static OuterEnumIntegerDefaultValue? fromJson(dynamic value) => OuterEnumIntegerDefaultValueTypeTransformer().decode(value);

  /// Returns a [List] containing instances of [OuterEnumIntegerDefaultValue]
  /// that were successfully decoded from the passed [JSON][json].
  static List<OuterEnumIntegerDefaultValue> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <OuterEnumIntegerDefaultValue>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = OuterEnumIntegerDefaultValue.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [OuterEnumIntegerDefaultValue] to int,
/// and [decode] dynamic data back to [OuterEnumIntegerDefaultValue].
class OuterEnumIntegerDefaultValueTypeTransformer {
  factory OuterEnumIntegerDefaultValueTypeTransformer() => _instance ??= const OuterEnumIntegerDefaultValueTypeTransformer._();

  const OuterEnumIntegerDefaultValueTypeTransformer._();

  /// Encodes this enum as a value suitable for JSON.
  int encode(OuterEnumIntegerDefaultValue data) => data._value;

  /// Returns the instance of [OuterEnumIntegerDefaultValue] that was successfully decoded
  /// from the passed [data] value on success, null otherwise.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  OuterEnumIntegerDefaultValue? decode(dynamic data, {bool allowNull = true}) {
    if (data is OuterEnumIntegerDefaultValue) {
      return data;
    }
    if (data != null) {
      switch (data) {
        case 0: return OuterEnumIntegerDefaultValue.number0;
        case 1: return OuterEnumIntegerDefaultValue.number1;
        case 2: return OuterEnumIntegerDefaultValue.number2;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// The singleton instance of this transformer.
  static OuterEnumIntegerDefaultValueTypeTransformer? _instance;
}

