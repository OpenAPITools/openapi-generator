//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class EnumClass {
  /// Instantiate a new enum with the provided [value].
  const EnumClass._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is EnumClass && other.value == value ||
      other is String && other == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => value;

  static const abc_ = EnumClass._('_abc');
  static const efg_ = EnumClass._('-efg');
  static const xyz_ = EnumClass._('(xyz)');

  /// List of all possible values in this [enum][EnumClass].
  static const values = <EnumClass>[
    abc_,
    efg_,
    xyz_,
  ];

  static EnumClass fromJson(dynamic value) =>
    EnumClassTypeTransformer().decode(value);

  static List<EnumClass> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumClass>[]
      : json
          .map((value) => EnumClass.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [EnumClass] to String,
/// and [decode] dynamic data back to [EnumClass].
class EnumClassTypeTransformer {
  const EnumClassTypeTransformer._();

  factory EnumClassTypeTransformer() => _instance ??= EnumClassTypeTransformer._();

  String encode(EnumClass data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumClass.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumClass decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case '_abc': return EnumClass.abc_;
      case '-efg': return EnumClass.efg_;
      case '(xyz)': return EnumClass.xyz_;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [EnumClassTypeTransformer] instance.
  static EnumClassTypeTransformer _instance;
}
