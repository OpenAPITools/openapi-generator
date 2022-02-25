//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class OuterEnumInteger {
  /// Instantiate a new enum with the provided [value].
  const OuterEnumInteger._(this.value);

  /// The underlying value of this enum member.
  final int value;

  @override
  String toString() => value.toString();

  int toJson() => value;

  static const number0 = OuterEnumInteger._(0);
  static const number1 = OuterEnumInteger._(1);
  static const number2 = OuterEnumInteger._(2);

  /// List of all possible values in this [enum][OuterEnumInteger].
  static const values = <OuterEnumInteger>[
    number0,
    number1,
    number2,
  ];

  static OuterEnumInteger? fromJson(dynamic value) => OuterEnumIntegerTypeTransformer().decode(value);

  static List<OuterEnumInteger>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <OuterEnumInteger>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = OuterEnumInteger.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [OuterEnumInteger] to int,
/// and [decode] dynamic data back to [OuterEnumInteger].
class OuterEnumIntegerTypeTransformer {
  factory OuterEnumIntegerTypeTransformer() => _instance ??= const OuterEnumIntegerTypeTransformer._();

  const OuterEnumIntegerTypeTransformer._();

  int encode(OuterEnumInteger data) => data.value;

  /// Decodes a [dynamic value][data] to a OuterEnumInteger.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  OuterEnumInteger? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data.toString()) {
        case 0: return OuterEnumInteger.number0;
        case 1: return OuterEnumInteger.number1;
        case 2: return OuterEnumInteger.number2;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [OuterEnumIntegerTypeTransformer] instance.
  static OuterEnumIntegerTypeTransformer? _instance;
}

