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
class ArrayOfArrayOfNumberOnly {
  /// Returns a new [ArrayOfArrayOfNumberOnly] instance.
  ArrayOfArrayOfNumberOnly({
    this.arrayArrayNumber = const [],
  });

  @JsonKey(
    defaultValue: const [],
    name: r'ArrayArrayNumber',
    required: false,
  )
  List<List<num>> arrayArrayNumber;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ArrayOfArrayOfNumberOnly &&
     other.arrayArrayNumber == arrayArrayNumber;

  @override
  int get hashCode =>
    (arrayArrayNumber == null ? 0 : arrayArrayNumber.hashCode);

  factory ArrayOfArrayOfNumberOnly.fromJson(Map<String, dynamic> json) => _$ArrayOfArrayOfNumberOnlyFromJson(json);

  Map<String, dynamic> toJson() => _$ArrayOfArrayOfNumberOnlyToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

