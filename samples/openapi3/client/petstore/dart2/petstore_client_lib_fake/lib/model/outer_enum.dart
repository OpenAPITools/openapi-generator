//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class OuterEnum {
  /// Instantiate a new enum with the provided [value].
  const OuterEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value ?? '';

  String toJson() => value;

  static const placed = OuterEnum._(r'placed');
  static const approved = OuterEnum._(r'approved');
  static const delivered = OuterEnum._(r'delivered');

  /// List of all possible values in this [enum][OuterEnum].
  static const values = <OuterEnum>[
    placed,
    approved,
    delivered,
  ];

  static OuterEnum fromJson(dynamic value) =>
    OuterEnumTypeTransformer().decode(value);

  static List<OuterEnum> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(OuterEnum.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <OuterEnum>[];
}

/// Transformation class that can [encode] an instance of [OuterEnum] to String,
/// and [decode] dynamic data back to [OuterEnum].
class OuterEnumTypeTransformer {
  factory OuterEnumTypeTransformer() => _instance ??= const OuterEnumTypeTransformer._();

  const OuterEnumTypeTransformer._();

  String encode(OuterEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a OuterEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  OuterEnum decode(dynamic data, {bool allowNull}) {
    if (data != null) {
      switch (data.toString()) {
        case r'placed': return OuterEnum.placed;
        case r'approved': return OuterEnum.approved;
        case r'delivered': return OuterEnum.delivered;
        default:
          if (allowNull == false) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [OuterEnumTypeTransformer] instance.
  static OuterEnumTypeTransformer _instance;
}

