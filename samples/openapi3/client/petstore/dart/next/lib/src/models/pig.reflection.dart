// Model reflection

part of 'pig.dart';


//class reflection

class PigReflection extends ClassReflection<Pig> {
  static PigReflection instanceGetter() => instance;
  static const instance = PigReflection._(
    modelName: r'Pig',
    className: r'Pig',
    discriminatorKey: r'className',
    discriminatorImplicitMappings: const {
      r'BasquePig': BasquePig.$reflection,
      r'DanishPig': DanishPig.$reflection,
    },
    discriminatorMappings: const {
      r'BasquePig': BasquePig.$reflection,
      r'DanishPig': DanishPig.$reflection,
    },
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: BasquePigReflection.instance,
    ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: DanishPigReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Pig, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const PigReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.additionalPropertiesPart,
  });




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
  List<PropertyReflection<Pig, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<Pig, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<Pig, 
            BasquePig
> oneOf0Part;
  
  final OneOfReflection<Pig, 
            DanishPig
> oneOf1Part;
  
  @override
  List<PartReflection<Pig, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Pig, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Pig, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Pig, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Pig.canDeserialize(src);
  @override
  Pig Function(Object? src) get deserializeFunction =>
      (src) => Pig.deserialize(src);

  @override
  Object? Function(Pig src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Pig.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Pig example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Pig(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    BasquePig.$reflection.example(discriminators: discriminators)
    


);
      }(),
      
    );
  }
}

class PigXmlReflection {
    const PigXmlReflection();
}

