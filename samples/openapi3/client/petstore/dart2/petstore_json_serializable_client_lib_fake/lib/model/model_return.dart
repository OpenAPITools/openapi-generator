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
class ModelReturn {
  /// Returns a new [ModelReturn] instance.
  ModelReturn({
    this.return_,
  });

  @JsonKey(
    nullable: false,
    name: r'return',
    required: false,
  )
  int return_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelReturn &&
     other.return_ == return_;

  @override
  int get hashCode =>
    (return_ == null ? 0 : return_.hashCode);

  factory ModelReturn.fromJson(Map<String, dynamic> json) => _$ModelReturnFromJson(json);

  Map<String, dynamic> toJson() => _$ModelReturnToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

