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
class ArrayTest {
  /// Returns a new [ArrayTest] instance.
  ArrayTest({
    this.arrayOfString = const [],
    this.arrayArrayOfInteger = const [],
    this.arrayArrayOfModel = const [],
  });

  @JsonKey(
    defaultValue: const [],
    name: r'array_of_string',
    required: false,
  )
  List<String> arrayOfString;

  @JsonKey(
    defaultValue: const [],
    name: r'array_array_of_integer',
    required: false,
  )
  List<List<int>> arrayArrayOfInteger;

  @JsonKey(
    defaultValue: const [],
    name: r'array_array_of_model',
    required: false,
  )
  List<List<ReadOnlyFirst>> arrayArrayOfModel;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ArrayTest &&
     other.arrayOfString == arrayOfString &&
     other.arrayArrayOfInteger == arrayArrayOfInteger &&
     other.arrayArrayOfModel == arrayArrayOfModel;

  @override
  int get hashCode =>
    (arrayOfString == null ? 0 : arrayOfString.hashCode) +
    (arrayArrayOfInteger == null ? 0 : arrayArrayOfInteger.hashCode) +
    (arrayArrayOfModel == null ? 0 : arrayArrayOfModel.hashCode);

  factory ArrayTest.fromJson(Map<String, dynamic> json) => _$ArrayTestFromJson(json);

  Map<String, dynamic> toJson() => _$ArrayTestToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

