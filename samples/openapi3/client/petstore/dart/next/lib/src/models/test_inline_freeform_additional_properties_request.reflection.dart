// Model reflection

part of 'test_inline_freeform_additional_properties_request.dart';


//class reflection

class TestInlineFreeformAdditionalPropertiesRequestReflection extends ClassReflection<TestInlineFreeformAdditionalPropertiesRequest> {
  static const instance = TestInlineFreeformAdditionalPropertiesRequestReflection._(
    someProperty: PropertyReflection(
      dartName: r'someProperty',
      nullable: false,
      required: false,
      oasName: r'someProperty',
      oasType: r'string',
      pattern: null,
    ),
  );
  const TestInlineFreeformAdditionalPropertiesRequestReflection._({
    required this.someProperty,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> someProperty;

  @override
  List<PropertyReflection> get members => [
    someProperty,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => TestInlineFreeformAdditionalPropertiesRequest.canDeserialize(src);
  @override
  TestInlineFreeformAdditionalPropertiesRequest Function(Object? src) get deserializeFunction =>
      (src) => TestInlineFreeformAdditionalPropertiesRequest.deserialize(src);

  @override
  Object? Function(TestInlineFreeformAdditionalPropertiesRequest src) get serializeFunction =>
      (src) => src.serialize();
}

class TestInlineFreeformAdditionalPropertiesRequestXmlReflection {
    const TestInlineFreeformAdditionalPropertiesRequestXmlReflection();
}

