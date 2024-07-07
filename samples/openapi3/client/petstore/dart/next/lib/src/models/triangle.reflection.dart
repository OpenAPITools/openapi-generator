// Model reflection

part of 'triangle.dart';


//class reflection

class TriangleReflection extends ClassReflection<Triangle> {
  static TriangleReflection instanceGetter() => instance;
  static const instance = TriangleReflection._(
    modelName: r'Triangle',
    className: r'Triangle',
    discriminatorKey: r'triangleType',
    discriminatorImplicitMappings: const {
      r'EquilateralTriangle': EquilateralTriangle.$reflection,
      r'IsoscelesTriangle': IsoscelesTriangle.$reflection,
      r'ScaleneTriangle': ScaleneTriangle.$reflection,
    },
    discriminatorMappings: const {
      r'EquilateralTriangle': EquilateralTriangle.$reflection,
      r'IsoscelesTriangle': IsoscelesTriangle.$reflection,
      r'ScaleneTriangle': ScaleneTriangle.$reflection,
    },
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: EquilateralTriangleReflection.instance,
    ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: IsoscelesTriangleReflection.instance,
    ),
    
    oneOf2Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: ScaleneTriangleReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Triangle, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const TriangleReflection._({
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
  List<PropertyReflection<Triangle, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<Triangle, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<Triangle, 
            EquilateralTriangle
> oneOf0Part;
  
  final OneOfReflection<Triangle, 
            IsoscelesTriangle
> oneOf1Part;
  
  final OneOfReflection<Triangle, 
            ScaleneTriangle
> oneOf2Part;
  
  @override
  List<PartReflection<Triangle, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Triangle, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Triangle, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,oneOf2Part,
  ];
  @override
  List<AnyOfReflection<Triangle, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Triangle.canDeserialize(src);
  @override
  Triangle Function(Object? src) get deserializeFunction =>
      (src) => Triangle.deserialize(src);

  @override
  Object? Function(Triangle src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Triangle.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Triangle example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Triangle(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    EquilateralTriangle.$reflection.example(discriminators: discriminators)
    


);
      }(),
      
    );
  }
}

class TriangleXmlReflection {
    const TriangleXmlReflection();
}

