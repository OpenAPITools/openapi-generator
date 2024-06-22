// Model reflection

part of 'test_json_form_data_request.dart';


//class reflection

class TestJsonFormDataRequestReflection extends ClassReflection<TestJsonFormDataRequest> {
  static const instance = TestJsonFormDataRequestReflection._(
    param: PropertyReflection(
      dartName: r'param',
      nullable: false,
      required: true,
      oasName: r'param',
      oasType: r'string',
      pattern: null,
    ),
    param2: PropertyReflection(
      dartName: r'param2',
      nullable: false,
      required: true,
      oasName: r'param2',
      oasType: r'string',
      pattern: null,
    ),
  );
  const TestJsonFormDataRequestReflection._({
    required this.param,
  
    required this.param2,
  });

  final PropertyReflection<
            String
> param;
  final PropertyReflection<
            String
> param2;

  @override
  List<PropertyReflection> get members => [
    param,
param2,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => TestJsonFormDataRequest.canDeserialize(src);
  @override
  TestJsonFormDataRequest Function(Object? src) get deserializeFunction =>
      (src) => TestJsonFormDataRequest.deserialize(src);

  @override
  Object? Function(TestJsonFormDataRequest src) get serializeFunction =>
      (src) => src.serialize();
}

class TestJsonFormDataRequestXmlReflection {
    const TestJsonFormDataRequestXmlReflection();
}

