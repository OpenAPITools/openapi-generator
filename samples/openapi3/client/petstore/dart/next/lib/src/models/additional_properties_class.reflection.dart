// Model reflection

part of 'additional_properties_class.dart';


//class reflection

class AdditionalPropertiesClassReflection extends ClassReflection<AdditionalPropertiesClass> {
  static const instance = AdditionalPropertiesClassReflection._(
    mapProperty: PropertyReflection(
      dartName: r'mapProperty',
      nullable: false,
      required: false,
      oasName: r'map_property',
      oasType: r'object',
      pattern: null,
    ),
    mapOfMapProperty: PropertyReflection(
      dartName: r'mapOfMapProperty',
      nullable: false,
      required: false,
      oasName: r'map_of_map_property',
      oasType: r'object',
      pattern: null,
    ),
    anytype1: PropertyReflection(
      dartName: r'anytype1',
      nullable: true,
      required: false,
      oasName: r'anytype_1',
      oasType: r'Object',
      pattern: null,
    ),
    mapWithUndeclaredPropertiesAnytype1: PropertyReflection(
      dartName: r'mapWithUndeclaredPropertiesAnytype1',
      nullable: false,
      required: false,
      oasName: r'map_with_undeclared_properties_anytype_1',
      oasType: r'object',
      pattern: null,
    ),
    mapWithUndeclaredPropertiesAnytype2: PropertyReflection(
      dartName: r'mapWithUndeclaredPropertiesAnytype2',
      nullable: false,
      required: false,
      oasName: r'map_with_undeclared_properties_anytype_2',
      oasType: r'object',
      pattern: null,
    ),
    mapWithUndeclaredPropertiesAnytype3: PropertyReflection(
      dartName: r'mapWithUndeclaredPropertiesAnytype3',
      nullable: false,
      required: false,
      oasName: r'map_with_undeclared_properties_anytype_3',
      oasType: r'object',
      pattern: null,
    ),
    emptyMap: PropertyReflection(
      dartName: r'emptyMap',
      nullable: false,
      required: false,
      oasName: r'empty_map',
      oasType: r'object',
      pattern: null,
    ),
    mapWithUndeclaredPropertiesString: PropertyReflection(
      dartName: r'mapWithUndeclaredPropertiesString',
      nullable: false,
      required: false,
      oasName: r'map_with_undeclared_properties_string',
      oasType: r'object',
      pattern: null,
    ),
  );
  const AdditionalPropertiesClassReflection._({
    required this.mapProperty,
  
    required this.mapOfMapProperty,
  
    required this.anytype1,
  
    required this.mapWithUndeclaredPropertiesAnytype1,
  
    required this.mapWithUndeclaredPropertiesAnytype2,
  
    required this.mapWithUndeclaredPropertiesAnytype3,
  
    required this.emptyMap,
  
    required this.mapWithUndeclaredPropertiesString,
  });

  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
            String
>
>> mapProperty;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
    Map<String, 
        
            String
>
>
>> mapOfMapProperty;
  final PropertyReflection<UndefinedWrapper<Object
?>> anytype1;
  final PropertyReflection<UndefinedWrapper<
            Map<String, Object?>
>> mapWithUndeclaredPropertiesAnytype1;
  final PropertyReflection<UndefinedWrapper<
            Map<String, Object?>
>> mapWithUndeclaredPropertiesAnytype2;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        Object
?>
>> mapWithUndeclaredPropertiesAnytype3;
  final PropertyReflection<UndefinedWrapper<
            Map<String, Object?>
>> emptyMap;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
            String
>
>> mapWithUndeclaredPropertiesString;

  @override
  List<PropertyReflection> get members => [
    mapProperty,
mapOfMapProperty,
anytype1,
mapWithUndeclaredPropertiesAnytype1,
mapWithUndeclaredPropertiesAnytype2,
mapWithUndeclaredPropertiesAnytype3,
emptyMap,
mapWithUndeclaredPropertiesString,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AdditionalPropertiesClass.canDeserialize(src);
  @override
  AdditionalPropertiesClass Function(Object? src) get deserializeFunction =>
      (src) => AdditionalPropertiesClass.deserialize(src);

  @override
  Object? Function(AdditionalPropertiesClass src) get serializeFunction =>
      (src) => src.serialize();
}

class AdditionalPropertiesClassXmlReflection {
    const AdditionalPropertiesClassXmlReflection();
}

