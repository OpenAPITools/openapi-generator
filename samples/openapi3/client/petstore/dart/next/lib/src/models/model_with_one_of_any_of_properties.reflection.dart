// Model reflection

part of 'model_with_one_of_any_of_properties.dart';


//class reflection

class ModelWithOneOfAnyOfPropertiesReflection extends ClassReflection<ModelWithOneOfAnyOfProperties> {
  static const instance = ModelWithOneOfAnyOfPropertiesReflection._(
    oneofProp: PropertyReflection(
      dartName: r'oneofProp',
      nullable: false,
      required: false,
      oasName: r'oneof_prop',
      oasType: r'ArrayOneOf',
      pattern: null,
    ),
    anyofProp: PropertyReflection(
      dartName: r'anyofProp',
      nullable: false,
      required: false,
      oasName: r'anyof_prop',
      oasType: r'ArrayAnyOf',
      pattern: null,
    ),
  );
  const ModelWithOneOfAnyOfPropertiesReflection._({
    required this.oneofProp,
  
    required this.anyofProp,
  });

  final PropertyReflection<UndefinedWrapper<
            ArrayOneOf
>> oneofProp;
  final PropertyReflection<UndefinedWrapper<
            ArrayAnyOf
>> anyofProp;

  @override
  List<PropertyReflection> get members => [
    oneofProp,
anyofProp,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ModelWithOneOfAnyOfProperties.canDeserialize(src);
  @override
  ModelWithOneOfAnyOfProperties Function(Object? src) get deserializeFunction =>
      (src) => ModelWithOneOfAnyOfProperties.deserialize(src);

  @override
  Object? Function(ModelWithOneOfAnyOfProperties src) get serializeFunction =>
      (src) => src.serialize();
}

class ModelWithOneOfAnyOfPropertiesXmlReflection {
    const ModelWithOneOfAnyOfPropertiesXmlReflection();
}

