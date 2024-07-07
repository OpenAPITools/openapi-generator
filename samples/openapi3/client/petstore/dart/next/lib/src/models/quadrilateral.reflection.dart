// Model reflection

part of 'quadrilateral.dart';


//class reflection

class QuadrilateralReflection extends ClassReflection<Quadrilateral> {
  static QuadrilateralReflection instanceGetter() => instance;
  static const instance = QuadrilateralReflection._(
    modelName: r'Quadrilateral',
    className: r'Quadrilateral',
    discriminatorKey: r'quadrilateralType',
    discriminatorImplicitMappings: const {
      r'ComplexQuadrilateral': ComplexQuadrilateral.$reflection,
      r'SimpleQuadrilateral': SimpleQuadrilateral.$reflection,
    },
    discriminatorMappings: const {
      r'ComplexQuadrilateral': ComplexQuadrilateral.$reflection,
      r'SimpleQuadrilateral': SimpleQuadrilateral.$reflection,
    },
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: SimpleQuadrilateralReflection.instance,
    ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: ComplexQuadrilateralReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Quadrilateral, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const QuadrilateralReflection._({
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
  List<PropertyReflection<Quadrilateral, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<Quadrilateral, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<Quadrilateral, 
            SimpleQuadrilateral
> oneOf0Part;
  
  final OneOfReflection<Quadrilateral, 
            ComplexQuadrilateral
> oneOf1Part;
  
  @override
  List<PartReflection<Quadrilateral, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Quadrilateral, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Quadrilateral, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Quadrilateral, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Quadrilateral.canDeserialize(src);
  @override
  Quadrilateral Function(Object? src) get deserializeFunction =>
      (src) => Quadrilateral.deserialize(src);

  @override
  Object? Function(Quadrilateral src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Quadrilateral.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Quadrilateral example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Quadrilateral(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    SimpleQuadrilateral.$reflection.example(discriminators: discriminators)
    


);
      }(),
      
    );
  }
}

class QuadrilateralXmlReflection {
    const QuadrilateralXmlReflection();
}

