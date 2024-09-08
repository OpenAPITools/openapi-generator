// Model reflection

part of 'mixed_properties_and_additional_properties_class.dart';


//class reflection

class MixedPropertiesAndAdditionalPropertiesClassReflection extends ModelReflection<MixedPropertiesAndAdditionalPropertiesClass> {
  static MixedPropertiesAndAdditionalPropertiesClassReflection instanceGetter() => instance;
  static const instance = MixedPropertiesAndAdditionalPropertiesClassReflection._(
    modelName: r'MixedPropertiesAndAdditionalPropertiesClass',
    className: r'MixedPropertiesAndAdditionalPropertiesClass',
    xml: XmlReflection(
),
    uuidPart: PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
            String
>>(
      dartName: r'uuid',
      nullable: false,
      required: false,
      oasName: r'uuid',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_uuidGetter),
      setter: FunctionWrapper2(_uuidSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    dateTimePart: PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
            DateTime
>>(
      dartName: r'dateTime',
      nullable: false,
      required: false,
      oasName: r'dateTime',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_dateTimeGetter),
      setter: FunctionWrapper2(_dateTimeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
),
    ),
    mapPart: PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
            Animal
>
>>(
      dartName: r'map',
      nullable: false,
      required: false,
      oasName: r'map',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_mapGetter),
      setter: FunctionWrapper2(_mapSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                Animal.$reflection
        
,
)
)
,
)
),
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const MixedPropertiesAndAdditionalPropertiesClassReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.uuidPart,
    required this.dateTimePart,
    required this.mapPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
            String
>> uuidPart;
  static UndefinedWrapper<
            String
> _uuidGetter(MixedPropertiesAndAdditionalPropertiesClass parent) {
    return parent.uuid;
  }
  static void _uuidSetter(MixedPropertiesAndAdditionalPropertiesClass parent, UndefinedWrapper<
            String
> value) {
    parent.uuid = value;
  }

  final PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
            DateTime
>> dateTimePart;
  static UndefinedWrapper<
            DateTime
> _dateTimeGetter(MixedPropertiesAndAdditionalPropertiesClass parent) {
    return parent.dateTime;
  }
  static void _dateTimeSetter(MixedPropertiesAndAdditionalPropertiesClass parent, UndefinedWrapper<
            DateTime
> value) {
    parent.dateTime = value;
  }

  final PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, UndefinedWrapper<
    Map<String, 
        
            Animal
>
>> mapPart;
  static UndefinedWrapper<
    Map<String, 
        
            Animal
>
> _mapGetter(MixedPropertiesAndAdditionalPropertiesClass parent) {
    return parent.map;
  }
  static void _mapSetter(MixedPropertiesAndAdditionalPropertiesClass parent, UndefinedWrapper<
    Map<String, 
        
            Animal
>
> value) {
    parent.map = value;
  }


  @override
  final Map<String, ModelReflection> discriminatorMappings;
  @override
  final Map<String, ModelReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;
  @override
  final XmlReflection xml;

  @override
  List<PropertyReflection<MixedPropertiesAndAdditionalPropertiesClass, dynamic>> get properties => [
    uuidPart,
dateTimePart,
mapPart,
  ];

  @override
  final AdditionalPropertiesPart<MixedPropertiesAndAdditionalPropertiesClass, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(MixedPropertiesAndAdditionalPropertiesClass instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(MixedPropertiesAndAdditionalPropertiesClass instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<MixedPropertiesAndAdditionalPropertiesClass, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  MixedPropertiesAndAdditionalPropertiesClass empty() {
    return MixedPropertiesAndAdditionalPropertiesClass(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is MixedPropertiesAndAdditionalPropertiesClassReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


