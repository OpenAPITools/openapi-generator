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
  includeIfNull: false,
  disallowUnrecognizedKeys: true,
)
class ModelList {
  /// Returns a new [ModelList] instance.
  ModelList({
    this.n123list,
  });

  @JsonKey(
    name: r'n123list',
    
    
    
  )
  String n123list;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelList &&
     other.n123list == n123list;

  @override
  int get hashCode =>
    (n123list == null ? 0 : n123list.hashCode);

  @override
  String toString() => toJson().toString();

  factory ModelList.fromJson(Map<String, dynamic> json) => _$ModelListFromJson(json);
  Map<String, dynamic> toJson() => _$ModelListToJson(this);
}

