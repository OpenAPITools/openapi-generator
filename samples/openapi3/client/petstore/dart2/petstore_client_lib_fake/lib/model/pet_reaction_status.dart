//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class PetReactionStatus {
  /// Instantiate a new enum with the provided [value].
  const PetReactionStatus._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const liked = PetReactionStatus._(r'liked');
  static const disliked = PetReactionStatus._(r'disliked');
  static const barked = PetReactionStatus._(r'barked');

  /// List of all possible values in this [enum][PetReactionStatus].
  static const values = <PetReactionStatus>[
    liked,
    disliked,
    barked,
  ];

  static PetReactionStatus? fromJson(dynamic value) => PetReactionStatusTypeTransformer().decode(value);

  static List<PetReactionStatus> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <PetReactionStatus>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = PetReactionStatus.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [PetReactionStatus] to String,
/// and [decode] dynamic data back to [PetReactionStatus].
class PetReactionStatusTypeTransformer {
  factory PetReactionStatusTypeTransformer() => _instance ??= const PetReactionStatusTypeTransformer._();

  const PetReactionStatusTypeTransformer._();

  String encode(PetReactionStatus data) => data.value;

  /// Decodes a [dynamic value][data] to a PetReactionStatus.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  PetReactionStatus? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data) {
        case r'liked': return PetReactionStatus.liked;
        case r'disliked': return PetReactionStatus.disliked;
        case r'barked': return PetReactionStatus.barked;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [PetReactionStatusTypeTransformer] instance.
  static PetReactionStatusTypeTransformer? _instance;
}

