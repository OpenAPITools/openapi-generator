//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';


enum TestEnum {
      @JsonValue(r'')
      empty(r''),
      @JsonValue(r'value_one')
      valueOne(r'value_one'),
      @JsonValue(r'value_two')
      valueTwo(r'value_two'),
      @JsonValue(r'unknown_default_open_api')
      unknownDefaultOpenApi(r'unknown_default_open_api');

  const TestEnum(this.value);

  final String value;

  @override
  String toString() => value;
}
