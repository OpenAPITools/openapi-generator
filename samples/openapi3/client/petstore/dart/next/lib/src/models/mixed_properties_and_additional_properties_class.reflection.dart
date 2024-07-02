// Model reflection

part of 'mixed_properties_and_additional_properties_class.dart';


//class reflection

class MixedPropertiesAndAdditionalPropertiesClassReflection extends ClassReflection<MixedPropertiesAndAdditionalPropertiesClass> {
  static const instance = MixedPropertiesAndAdditionalPropertiesClassReflection._(
    uuid: PropertyReflection(
      dartName: r'uuid',
      nullable: false,
      required: false,
      oasName: r'uuid',
      oasType: r'string',
      pattern: null,
    ),
    dateTime: PropertyReflection(
      dartName: r'dateTime',
      nullable: false,
      required: false,
      oasName: r'dateTime',
      oasType: r'string',
      pattern: null,
    ),
    map: PropertyReflection(
      dartName: r'map',
      nullable: false,
      required: false,
      oasName: r'map',
      oasType: r'object',
      pattern: null,
    ),
  );
  const MixedPropertiesAndAdditionalPropertiesClassReflection._({
    required this.uuid,
  
    required this.dateTime,
  
    required this.map,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> uuid;
  final PropertyReflection<UndefinedWrapper<
            DateTime
>> dateTime;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
            Animal
>
>> map;

  @override
  List<PropertyReflection> get members => [
    uuid,
dateTime,
map,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => MixedPropertiesAndAdditionalPropertiesClass.canDeserialize(src);
  @override
  MixedPropertiesAndAdditionalPropertiesClass Function(Object? src) get deserializeFunction =>
      (src) => MixedPropertiesAndAdditionalPropertiesClass.deserialize(src);

  @override
  Object? Function(MixedPropertiesAndAdditionalPropertiesClass src) get serializeFunction =>
      (src) => src.serialize();
}

class MixedPropertiesAndAdditionalPropertiesClassXmlReflection {
    const MixedPropertiesAndAdditionalPropertiesClassXmlReflection();
}

