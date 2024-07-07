// Model reflection

part of 'enum_string_discriminator.dart';


//class reflection

class EnumStringDiscriminatorReflection extends ClassReflection<EnumStringDiscriminator> {
  static EnumStringDiscriminatorReflection instanceGetter() => instance;
  static const instance = EnumStringDiscriminatorReflection._(
    modelName: r'EnumStringDiscriminator',
    className: r'EnumStringDiscriminator',
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
      getter: _enumStrTypeGetter,
      setter: _enumStrTypeSetter,
    ),
    discriminatorKey: r'enum_str_type',
    discriminatorImplicitMappings: const {
    },
    discriminatorMappings: const {
    },
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<EnumStringDiscriminator, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const EnumStringDiscriminatorReflection._({
    required this.modelName,
    required this.className,
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
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<EnumStringDiscriminator, dynamic>> get properties => [
    enumStrTypePart,
  ];

  final AdditionalPropertiesReflection<EnumStringDiscriminator, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<EnumStringDiscriminator, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<EnumStringDiscriminator, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => EnumStringDiscriminator.canDeserialize(src);
  @override
  EnumStringDiscriminator Function(Object? src) get deserializeFunction =>
      (src) => EnumStringDiscriminator.deserialize(src);

  @override
  Object? Function(EnumStringDiscriminator src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of EnumStringDiscriminator.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  EnumStringDiscriminator example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return EnumStringDiscriminator(
      enumStrType: () {
        PartReflection? _partReflection = _reflection.enumStrTypePart;
        
        return 


            exampleEnum(EnumStringDiscriminatorEnumStrTypeEnum.values)



;
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class EnumStringDiscriminatorXmlReflection {
    const EnumStringDiscriminatorXmlReflection();
}

