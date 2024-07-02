// Model reflection

part of 'foo_get_default_response.dart';


//class reflection

class FooGetDefaultResponseReflection extends ClassReflection<FooGetDefaultResponse> {
  static const instance = FooGetDefaultResponseReflection._(
    string: PropertyReflection(
      dartName: r'string',
      nullable: false,
      required: false,
      oasName: r'string',
      oasType: r'Foo',
      pattern: null,
    ),
  );
  const FooGetDefaultResponseReflection._({
    required this.string,
  });

  final PropertyReflection<UndefinedWrapper<
            Foo
>> string;

  @override
  List<PropertyReflection> get members => [
    string,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FooGetDefaultResponse.canDeserialize(src);
  @override
  FooGetDefaultResponse Function(Object? src) get deserializeFunction =>
      (src) => FooGetDefaultResponse.deserialize(src);

  @override
  Object? Function(FooGetDefaultResponse src) get serializeFunction =>
      (src) => src.serialize();
}

class FooGetDefaultResponseXmlReflection {
    const FooGetDefaultResponseXmlReflection();
}

