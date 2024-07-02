// Model reflection

part of 'foo.dart';


//class reflection

class FooReflection extends ClassReflection<Foo> {
  static const instance = FooReflection._(
    bar: PropertyReflection(
      dartName: r'bar',
      nullable: false,
      required: false,
      oasName: r'bar',
      oasType: r'string',
      pattern: null,
    ),
  );
  const FooReflection._({
    required this.bar,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> bar;

  @override
  List<PropertyReflection> get members => [
    bar,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Foo.canDeserialize(src);
  @override
  Foo Function(Object? src) get deserializeFunction =>
      (src) => Foo.deserialize(src);

  @override
  Object? Function(Foo src) get serializeFunction =>
      (src) => src.serialize();
}

class FooXmlReflection {
    const FooXmlReflection();
}

