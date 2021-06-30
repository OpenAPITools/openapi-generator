//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class OuterEnumDefaultValue {
  /// Instantiate a new enum with the provided [value].
  const OuterEnumDefaultValue._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const placed = OuterEnumDefaultValue._(r'placed');
  static const approved = OuterEnumDefaultValue._(r'approved');
  static const delivered = OuterEnumDefaultValue._(r'delivered');

  /// List of all possible values in this [enum][OuterEnumDefaultValue].
  static const values = <OuterEnumDefaultValue>[
    placed,
    approved,
    delivered,
  ];

  static OuterEnumDefaultValue fromJson(dynamic value) =>
    OuterEnumDefaultValueTypeTransformer().decode(value);

  static List<OuterEnumDefaultValue> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <OuterEnumDefaultValue>[]
      : json
          .map((value) => OuterEnumDefaultValue.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [OuterEnumDefaultValue] to String,
/// and [decode] dynamic data back to [OuterEnumDefaultValue].
class OuterEnumDefaultValueTypeTransformer {
  const OuterEnumDefaultValueTypeTransformer._();

  factory OuterEnumDefaultValueTypeTransformer() => _instance ??= OuterEnumDefaultValueTypeTransformer._();

  String encode(OuterEnumDefaultValue data) => data.value;

  /// Decodes a [dynamic value][data] to a OuterEnumDefaultValue.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  OuterEnumDefaultValue decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case r'placed': return OuterEnumDefaultValue.placed;
      case r'approved': return OuterEnumDefaultValue.approved;
      case r'delivered': return OuterEnumDefaultValue.delivered;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [OuterEnumDefaultValueTypeTransformer] instance.
  static OuterEnumDefaultValueTypeTransformer _instance;
}
