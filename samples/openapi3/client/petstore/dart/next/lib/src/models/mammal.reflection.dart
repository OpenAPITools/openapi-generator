// Model reflection

part of 'mammal.dart';


//class reflection

class MammalReflection extends ClassReflection<Mammal> {
  static MammalReflection instanceGetter() => instance;
  static const instance = MammalReflection._(
    modelName: r'mammal',
    className: r'Mammal',
    discriminatorKey: r'className',
    discriminatorImplicitMappings: const {
      r'Pig': Pig.$reflection,
      r'Whale': Whale.$reflection,
      r'Zebra': Zebra.$reflection,
    },
    discriminatorMappings: const {
      r'Pig': Pig.$reflection,
      r'whale': Whale.$reflection,
      r'zebra': Zebra.$reflection,
    },
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: WhaleReflection.instance,
    ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: ZebraReflection.instance,
    ),
    
    oneOf2Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: PigReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Mammal, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const MammalReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.oneOf2Part,
    
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
  List<PropertyReflection<Mammal, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<Mammal, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<Mammal, 
            Whale
> oneOf0Part;
  
  final OneOfReflection<Mammal, 
            Zebra
> oneOf1Part;
  
  final OneOfReflection<Mammal, 
            Pig
> oneOf2Part;
  
  @override
  List<PartReflection<Mammal, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Mammal, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Mammal, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,oneOf2Part,
  ];
  @override
  List<AnyOfReflection<Mammal, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Mammal.canDeserialize(src);
  @override
  Mammal Function(Object? src) get deserializeFunction =>
      (src) => Mammal.deserialize(src);

  @override
  Object? Function(Mammal src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Mammal.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Mammal example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Mammal(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    Whale.$reflection.example(discriminators: discriminators)
    


);
      }(),
      
    );
  }
}

class MammalXmlReflection {
    const MammalXmlReflection();
}

