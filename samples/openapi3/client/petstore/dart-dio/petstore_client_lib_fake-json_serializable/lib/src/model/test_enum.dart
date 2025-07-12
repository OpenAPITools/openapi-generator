//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';


enum TestEnum {
      @JsonValue(r'')
      empty(r''),
      @JsonValue(r'1')
      n1(r'1'),
      @JsonValue(r'2')
      n2(r'2'),
      @JsonValue(r'unknown_default_open_api')
      unknownDefaultOpenApi(r'unknown_default_open_api');

  const TestEnum(this.value);

  final String value;

  @override
  String toString() => value;
}
