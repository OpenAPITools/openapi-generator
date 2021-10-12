//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class OuterEnumDefaultValue {
  /// Instantiate a new enum with the provided [value].
  const OuterEnumDefaultValue._(this.value);

  /// The underlying value of this enum member.
  final String? value;

  @override
  String toString() => value ?? '';

  String? toJson() => value;

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

  static List<OuterEnumDefaultValue> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<OuterEnumDefaultValue>((i) => OuterEnumDefaultValue.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <OuterEnumDefaultValue>[];
}

/// Transformation class that can [encode] an instance of [OuterEnumDefaultValue] to String,
/// and [decode] dynamic data back to [OuterEnumDefaultValue].
class OuterEnumDefaultValueTypeTransformer {
  factory OuterEnumDefaultValueTypeTransformer() => _instance ??= const OuterEnumDefaultValueTypeTransformer._();

  const OuterEnumDefaultValueTypeTransformer._();

  String? encode(OuterEnumDefaultValue data) => data.value;

  /// Decodes a [dynamic value][data] to a OuterEnumDefaultValue.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  OuterEnumDefaultValue decode(dynamic data) {
    if (data == r'placed') {
      return OuterEnumDefaultValue.placed;
    }
    if (data == r'approved') {
      return OuterEnumDefaultValue.approved;
    }
    if (data == r'delivered') {
      return OuterEnumDefaultValue.delivered;
    }
    throw ArgumentError('Unknown enum value to decode: $data');

  }

  /// Singleton [OuterEnumDefaultValueTypeTransformer] instance.
  static OuterEnumDefaultValueTypeTransformer? _instance;
}

