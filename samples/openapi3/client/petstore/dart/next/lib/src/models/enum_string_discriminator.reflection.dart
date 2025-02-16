// Model reflection

part of 'enum_string_discriminator.dart';


//class reflection

class EnumStringDiscriminatorReflection extends ModelReflection<EnumStringDiscriminator> {
  static EnumStringDiscriminatorReflection instanceGetter() => instance;
  static const instance = EnumStringDiscriminatorReflection._(
    modelName: r'EnumStringDiscriminator',
    className: r'EnumStringDiscriminator',
    xml: XmlReflection(
),
    enumStrTypePart: PropertyReflection<EnumStringDiscriminator, 
            EnumStringDiscriminatorEnumStrTypeEnum
>(
      dartName: r'enumStrType',
      nullable: false,
      required: true,
      oasName: r'enum_str_type',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_enumStrTypeGetter),
      setter: FunctionWrapper2(_enumStrTypeSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            EnumStringDiscriminatorEnumStrTypeEnum.$reflection
        
        
,
)
,
    ),
    discriminatorKey: r'enum_str_type',
    discriminatorImplicitMappings: const {
    },
    discriminatorMappings: const {
    },
    
    
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
  const EnumStringDiscriminatorReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.enumStrTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<EnumStringDiscriminator, 
            EnumStringDiscriminatorEnumStrTypeEnum
> enumStrTypePart;
  static 
            EnumStringDiscriminatorEnumStrTypeEnum
 _enumStrTypeGetter(EnumStringDiscriminator parent) {
    return parent.enumStrType;
  }
  static void _enumStrTypeSetter(EnumStringDiscriminator parent, 
            EnumStringDiscriminatorEnumStrTypeEnum
 value) {
    parent.enumStrType = value;
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
  List<PropertyReflection<EnumStringDiscriminator, dynamic>> get properties => [
    enumStrTypePart,
  ];

  @override
  final AdditionalPropertiesPart<EnumStringDiscriminator, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(EnumStringDiscriminator instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(EnumStringDiscriminator instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<EnumStringDiscriminator, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  EnumStringDiscriminator empty() {
    return EnumStringDiscriminator(
      enumStrType: enumStrTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is EnumStringDiscriminatorReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


