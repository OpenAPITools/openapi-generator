//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class DogAllOf {
  /// Returns a new [DogAllOf] instance.
  DogAllOf({
    this.breed,
  });

  @JsonKey(
    nullable: false,
    name: r'breed',
    required: false,
  )
  String breed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is DogAllOf &&
     other.breed == breed;

  @override
  int get hashCode =>
    (breed == null ? 0 : breed.hashCode);

  factory DogAllOf.fromJson(Map<String, dynamic> json) => _$DogAllOfFromJson(json);

  Map<String, dynamic> toJson() => _$DogAllOfToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

