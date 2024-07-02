// Model reflection

part of 'test_enum_parameters_request.dart';


//class reflection

class TestEnumParametersRequestReflection extends ClassReflection<TestEnumParametersRequest> {
  static const instance = TestEnumParametersRequestReflection._(
    enumFormStringArray: PropertyReflection(
      dartName: r'enumFormStringArray',
      nullable: false,
      required: false,
      oasName: r'enum_form_string_array',
      oasType: r'array',
      pattern: null,
    ),
    enumFormString: PropertyReflection(
      dartName: r'enumFormString',
      nullable: false,
      required: false,
      oasName: r'enum_form_string',
      oasType: r'string',
      pattern: null,
    ),
  );
  const TestEnumParametersRequestReflection._({
    required this.enumFormStringArray,
  
    required this.enumFormString,
  });

  final PropertyReflection<UndefinedWrapper<
    List<
        
            TestEnumParametersRequestEnumFormStringArrayEnum
>
>> enumFormStringArray;
  final PropertyReflection<UndefinedWrapper<
            TestEnumParametersRequestEnumFormStringEnum
>> enumFormString;

  @override
  List<PropertyReflection> get members => [
    enumFormStringArray,
enumFormString,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => TestEnumParametersRequest.canDeserialize(src);
  @override
  TestEnumParametersRequest Function(Object? src) get deserializeFunction =>
      (src) => TestEnumParametersRequest.deserialize(src);

  @override
  Object? Function(TestEnumParametersRequest src) get serializeFunction =>
      (src) => src.serialize();
}

class TestEnumParametersRequestXmlReflection {
    const TestEnumParametersRequestXmlReflection();
}

