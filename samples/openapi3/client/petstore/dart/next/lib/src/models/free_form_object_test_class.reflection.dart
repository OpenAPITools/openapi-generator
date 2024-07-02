// Model reflection

part of 'free_form_object_test_class.dart';


//class reflection

class FreeFormObjectTestClassReflection extends ClassReflection<FreeFormObjectTestClass> {
  static const instance = FreeFormObjectTestClassReflection._(
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
    properties: PropertyReflection(
      dartName: r'properties',
      nullable: false,
      required: false,
      oasName: r'properties',
      oasType: r'FreeFormObjectTestClassProperties',
      pattern: null,
    ),
  );
  const FreeFormObjectTestClassReflection._({
    required this.name,
  
    required this.properties,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> name;
  final PropertyReflection<UndefinedWrapper<
            FreeFormObjectTestClassProperties
>> properties;

  @override
  List<PropertyReflection> get members => [
    name,
properties,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => FreeFormObjectTestClass.canDeserialize(src);
  @override
  FreeFormObjectTestClass Function(Object? src) get deserializeFunction =>
      (src) => FreeFormObjectTestClass.deserialize(src);

  @override
  Object? Function(FreeFormObjectTestClass src) get serializeFunction =>
      (src) => src.serialize();
}

class FreeFormObjectTestClassXmlReflection {
    const FreeFormObjectTestClassXmlReflection();
}

