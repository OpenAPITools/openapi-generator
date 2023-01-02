//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'test_query_style_form_explode_true_array_string_query_object_parameter.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter {
  /// Returns a new [TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter] instance.
  TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter({
    this.values,
  });

  @JsonKey(name: r'values', required: false, includeIfNull: false)
  final List<String>? values;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter &&
          other.values == values;

  @override
  int get hashCode => values.hashCode;

  factory TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter.fromJson(
          Map<String, dynamic> json) =>
      _$TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterFromJson(
          json);

  Map<String, dynamic> toJson() =>
      _$TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterToJson(
          this);

  @override
  String toString() {
    return toJson().toString();
  }
}
