// Model reflection

part of 'has_only_read_only.dart';


//class reflection

class HasOnlyReadOnlyReflection extends ClassReflection<HasOnlyReadOnly> {
  static const instance = HasOnlyReadOnlyReflection._(
    bar: PropertyReflection(
      dartName: r'bar',
      nullable: false,
      required: false,
      oasName: r'bar',
      oasType: r'string',
      pattern: null,
    ),
    foo: PropertyReflection(
      dartName: r'foo',
      nullable: false,
      required: false,
      oasName: r'foo',
      oasType: r'string',
      pattern: null,
    ),
  );
  const HasOnlyReadOnlyReflection._({
    required this.bar,
  
    required this.foo,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> bar;
  final PropertyReflection<UndefinedWrapper<
            String
>> foo;

  @override
  List<PropertyReflection> get members => [
    bar,
foo,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => HasOnlyReadOnly.canDeserialize(src);
  @override
  HasOnlyReadOnly Function(Object? src) get deserializeFunction =>
      (src) => HasOnlyReadOnly.deserialize(src);

  @override
  Object? Function(HasOnlyReadOnly src) get serializeFunction =>
      (src) => src.serialize();
}

class HasOnlyReadOnlyXmlReflection {
    const HasOnlyReadOnlyXmlReflection();
}

