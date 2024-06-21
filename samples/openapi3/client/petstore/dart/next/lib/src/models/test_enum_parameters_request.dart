// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'test_enum_parameters_request.reflection.dart';
part 'test_enum_parameters_request.serialization.dart';


/// TestEnumParametersRequestMixin
///
/// Properties:
/// * [enumFormStringArray] - Form parameter enum test (string array)
/// * [enumFormString] - Form parameter enum test (string)
mixin TestEnumParametersRequestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<List<TestEnumParametersRequestEnumFormStringArrayEnum>> get enumFormStringArray;
  UndefinedWrapper<TestEnumParametersRequestEnumFormStringEnum> get enumFormString;

}

/// TestEnumParametersRequest
///
/// Properties:
/// * [enumFormStringArray] - Form parameter enum test (string array)
/// * [enumFormString] - Form parameter enum test (string)
class TestEnumParametersRequest with
$OpenApiObjectMixin,


TestEnumParametersRequestMixin {
  @override
  UndefinedWrapper<List<TestEnumParametersRequestEnumFormStringArrayEnum>> enumFormStringArray;
  @override
  UndefinedWrapper<TestEnumParametersRequestEnumFormStringEnum> enumFormString;





  TestEnumParametersRequest.$all({
    required this.enumFormStringArray,
    required this.enumFormString,
    
    
  });

  TestEnumParametersRequest({
    this.enumFormStringArray = const UndefinedWrapper.undefined(),
    this.enumFormString = const UndefinedWrapper('-efg'),
    
    
  });
}




extension type const TestEnumParametersRequestEnumFormStringArrayEnum._(String value) {
  /// Form parameter enum test (string array)
      const TestEnumParametersRequestEnumFormStringArrayEnum.greaterThan() : this._(r'>');
  /// Form parameter enum test (string array)
      const TestEnumParametersRequestEnumFormStringArrayEnum.value() : this._(r'$');

  /// Creates a [TestEnumParametersRequestEnumFormStringArrayEnum] enum from a value and safely checking if it exists.
  factory TestEnumParametersRequestEnumFormStringArrayEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [TestEnumParametersRequestEnumFormStringArrayEnum] enum from a value without checking if it exists.
  const TestEnumParametersRequestEnumFormStringArrayEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<TestEnumParametersRequestEnumFormStringArrayEnum> values = [
    TestEnumParametersRequestEnumFormStringArrayEnum.greaterThan(),
    TestEnumParametersRequestEnumFormStringArrayEnum.value(),
    
  ];
}

extension type const TestEnumParametersRequestEnumFormStringEnum._(String value) {
  /// Form parameter enum test (string)
      const TestEnumParametersRequestEnumFormStringEnum.abc() : this._(r'_abc');
  /// Form parameter enum test (string)
      const TestEnumParametersRequestEnumFormStringEnum.efg() : this._(r'-efg');
  /// Form parameter enum test (string)
      const TestEnumParametersRequestEnumFormStringEnum.xyz() : this._(r'(xyz)');

  /// Creates a [TestEnumParametersRequestEnumFormStringEnum] enum from a value and safely checking if it exists.
  factory TestEnumParametersRequestEnumFormStringEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [TestEnumParametersRequestEnumFormStringEnum] enum from a value without checking if it exists.
  const TestEnumParametersRequestEnumFormStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<TestEnumParametersRequestEnumFormStringEnum> values = [
    TestEnumParametersRequestEnumFormStringEnum.abc(),
    TestEnumParametersRequestEnumFormStringEnum.efg(),
    TestEnumParametersRequestEnumFormStringEnum.xyz(),
    
  ];
}

