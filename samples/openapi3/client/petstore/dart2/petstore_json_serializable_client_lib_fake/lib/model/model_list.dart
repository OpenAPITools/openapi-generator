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
class ModelList {
  /// Returns a new [ModelList] instance.
  ModelList({
    this.n123list,
  });

  @JsonKey(
    nullable: false,
    name: r'123-list',
    required: false,
  )
  String n123list;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelList &&
     other.n123list == n123list;

  @override
  int get hashCode =>
    (n123list == null ? 0 : n123list.hashCode);

  factory ModelList.fromJson(Map<String, dynamic> json) => _$ModelListFromJson(json);

  Map<String, dynamic> toJson() => _$ModelListToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

