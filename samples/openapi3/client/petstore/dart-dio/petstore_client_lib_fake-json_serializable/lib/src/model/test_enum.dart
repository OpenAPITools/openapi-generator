//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

enum TestEnum {
  /**
   * line1
   * line2
   * 
   */
  @JsonValue(r'')
  empty(r''),
  /**
   * line3
   * line4
   * 
   */
  @JsonValue(r'value_one')
  valueOne(r'value_one'),
  /**
   * line5
   * line6
   * 
   */
  @JsonValue(r'value_two')
  valueTwo(r'value_two'),
  @JsonValue(r'unknown_default_open_api')
  unknownDefaultOpenApi(r'unknown_default_open_api');

  const TestEnum(this.value);

  final String value;

  @override
  String toString() => value;
}
