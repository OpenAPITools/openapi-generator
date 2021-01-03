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
class ModelReturn {
  /// Returns a new [ModelReturn] instance.
  ModelReturn({
    this.return_,
  });

  @JsonKey(
    name: r'return_',
    
    
    
  )
  int return_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelReturn &&
     other.return_ == return_;

  @override
  int get hashCode =>
    (return_ == null ? 0 : return_.hashCode);

  @override
  String toString() => toJson().toString();

  factory ModelReturn.fromJson(Map<String, dynamic> json) => _$ModelReturnFromJson(json);
  Map<String, dynamic> toJson() => _$ModelReturnToJson(this);
}

