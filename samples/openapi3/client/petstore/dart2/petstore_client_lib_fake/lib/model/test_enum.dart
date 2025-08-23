//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class TestEnum {
  /// Instantiate a new enum with the provided [value].
  const TestEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const empty = TestEnum._(r'');
  static const n1 = TestEnum._(r'1');
  static const n2 = TestEnum._(r'2');

  /// List of all possible values in this [enum][TestEnum].
  static const values = <TestEnum>[
    empty,
    n1,
    n2,
  ];

  static TestEnum? fromJson(dynamic value) => TestEnumTypeTransformer().decode(value);

  static List<TestEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <TestEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = TestEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [TestEnum] to String,
/// and [decode] dynamic data back to [TestEnum].
class TestEnumTypeTransformer {
  factory TestEnumTypeTransformer() => _instance ??= const TestEnumTypeTransformer._();

  const TestEnumTypeTransformer._();

  String encode(TestEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a TestEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  TestEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data) {
        case r'': return TestEnum.empty;
        case r'1': return TestEnum.n1;
        case r'2': return TestEnum.n2;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [TestEnumTypeTransformer] instance.
  static TestEnumTypeTransformer? _instance;
}

