// Model reflection

part of 'special_model_name.dart';


//class reflection

class SpecialModelNameReflection extends ClassReflection<SpecialModelName> {
  static const instance = SpecialModelNameReflection._(
    $specialPropertyName: PropertyReflection(
      dartName: r'$specialPropertyName',
      nullable: false,
      required: false,
      oasName: r'$special[property.name]',
      oasType: r'integer',
      pattern: null,
    ),
    specialModelName: PropertyReflection(
      dartName: r'specialModelName',
      nullable: false,
      required: false,
      oasName: r'_special_model.name_',
      oasType: r'string',
      pattern: null,
    ),
  );
  const SpecialModelNameReflection._({
    required this.$specialPropertyName,
  
    required this.specialModelName,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> $specialPropertyName;
  final PropertyReflection<UndefinedWrapper<
            String
>> specialModelName;

  @override
  List<PropertyReflection> get members => [
    $specialPropertyName,
specialModelName,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => SpecialModelName.canDeserialize(src);
  @override
  SpecialModelName Function(Object? src) get deserializeFunction =>
      (src) => SpecialModelName.deserialize(src);

  @override
  Object? Function(SpecialModelName src) get serializeFunction =>
      (src) => src.serialize();
}

class SpecialModelNameXmlReflection {
    const SpecialModelNameXmlReflection();
}

