// Model def

import 'package:petstore_api/_internal.dart';


part 'test_enum_parameters_request.reflection.dart';
part 'test_enum_parameters_request.serialization.dart';


/// TestEnumParametersRequestMixin
///
/// Properties:
/// * [enumFormStringArray] - Form parameter enum test (string array)
/// * [enumFormString] - Form parameter enum test (string)
mixin TestEnumParametersRequestMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    List<
        
            TestEnumParametersRequestEnumFormStringArrayEnum

>

> get enumFormStringArray;
UndefinedWrapper<
            TestEnumParametersRequestEnumFormStringEnum

> get enumFormString;
  
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
  UndefinedWrapper<
    List<
        
            TestEnumParametersRequestEnumFormStringArrayEnum

>

> enumFormStringArray;
  @override
  UndefinedWrapper<
            TestEnumParametersRequestEnumFormStringEnum

> enumFormString;

  AdditionalProperties<Object

?> additionalProperties;

  

  TestEnumParametersRequest.$all({
        required this.enumFormStringArray,
    required this.enumFormString,
    required this.additionalProperties,
    
  });

  TestEnumParametersRequest({
      this.enumFormStringArray = const UndefinedWrapper
        .undefined()
,
  this.enumFormString = const UndefinedWrapper
    (
        TestEnumParametersRequestEnumFormStringEnum.$unsafe('-efg')
        
    )
    
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = TestEnumParametersRequestReflection.instance;
  TestEnumParametersRequestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$TestEnumParametersRequestToMap(this);
  }
  factory TestEnumParametersRequest.fromMap(Map<String, dynamic> src) {
    return _$TestEnumParametersRequestFromMap(src);
  }
  static TestEnumParametersRequest? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return TestEnumParametersRequest.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$TestEnumParametersRequestCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory TestEnumParametersRequest.deserialize(Object? src) {
    return _$TestEnumParametersRequestDeserialize(src);
  }
  static TestEnumParametersRequest? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return TestEnumParametersRequest.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$TestEnumParametersRequestCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$TestEnumParametersRequestSerialize(this);
  }
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

  static bool canDeserialize(Object? value) {
    return value is String && values.where((element) => element.value == value).firstOrNull != null;
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

  static bool canDeserialize(Object? value) {
    return value is String && values.where((element) => element.value == value).firstOrNull != null;
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

