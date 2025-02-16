// Model def

import 'package:petstore_api/_internal.dart';


part 'test_enum_parameters_request.reflection.dart';


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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = TestEnumParametersRequestReflection.instance;
  TestEnumParametersRequestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory TestEnumParametersRequest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  TestEnumParametersRequest clone() {
    return $reflection.clone(this);
  }
}


extension type const TestEnumParametersRequestEnumFormStringArrayEnum._(String value) implements String {
      const TestEnumParametersRequestEnumFormStringArrayEnum.greaterThan() : this._(r'>');
      const TestEnumParametersRequestEnumFormStringArrayEnum.value() : this._(r'$');

  /// Creates a [TestEnumParametersRequestEnumFormStringArrayEnum] enum from a value and safely checking if it exists.
  factory TestEnumParametersRequestEnumFormStringArrayEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<TestEnumParametersRequestEnumFormStringArrayEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'greaterThan', oasValue: r'>', value: TestEnumParametersRequestEnumFormStringArrayEnum.greaterThan()),
      
        EnumMemberReflection(dartName: r'value', oasValue: r'$', value: TestEnumParametersRequestEnumFormStringArrayEnum.value()),
      
    ],
  );

  factory TestEnumParametersRequestEnumFormStringArrayEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [TestEnumParametersRequestEnumFormStringArrayEnum] enum from a value without checking if it exists.
  const TestEnumParametersRequestEnumFormStringArrayEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<TestEnumParametersRequestEnumFormStringArrayEnum> values = [
    TestEnumParametersRequestEnumFormStringArrayEnum.greaterThan(),
    TestEnumParametersRequestEnumFormStringArrayEnum.value(),
    
  ];
}




extension type const TestEnumParametersRequestEnumFormStringEnum._(String value) implements String {
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

  static const $reflection = EnumReflection<TestEnumParametersRequestEnumFormStringEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'abc', oasValue: r'_abc', value: TestEnumParametersRequestEnumFormStringEnum.abc()),
      
        EnumMemberReflection(dartName: r'efg', oasValue: r'-efg', value: TestEnumParametersRequestEnumFormStringEnum.efg()),
      
        EnumMemberReflection(dartName: r'xyz', oasValue: r'(xyz)', value: TestEnumParametersRequestEnumFormStringEnum.xyz()),
      
    ],
  );

  factory TestEnumParametersRequestEnumFormStringEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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


