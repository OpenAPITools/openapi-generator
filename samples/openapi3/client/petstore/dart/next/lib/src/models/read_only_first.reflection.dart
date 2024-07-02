// Model reflection

part of 'read_only_first.dart';


//class reflection

class ReadOnlyFirstReflection extends ClassReflection<ReadOnlyFirst> {
  static const instance = ReadOnlyFirstReflection._(
    bar: PropertyReflection(
      dartName: r'bar',
      nullable: false,
      required: false,
      oasName: r'bar',
      oasType: r'string',
      pattern: null,
    ),
    baz: PropertyReflection(
      dartName: r'baz',
      nullable: false,
      required: false,
      oasName: r'baz',
      oasType: r'string',
      pattern: null,
    ),
  );
  const ReadOnlyFirstReflection._({
    required this.bar,
  
    required this.baz,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> bar;
  final PropertyReflection<UndefinedWrapper<
            String
>> baz;

  @override
  List<PropertyReflection> get members => [
    bar,
baz,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ReadOnlyFirst.canDeserialize(src);
  @override
  ReadOnlyFirst Function(Object? src) get deserializeFunction =>
      (src) => ReadOnlyFirst.deserialize(src);

  @override
  Object? Function(ReadOnlyFirst src) get serializeFunction =>
      (src) => src.serialize();
}

class ReadOnlyFirstXmlReflection {
    const ReadOnlyFirstXmlReflection();
}

