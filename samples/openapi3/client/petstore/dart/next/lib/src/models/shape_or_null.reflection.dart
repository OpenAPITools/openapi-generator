// Model reflection

part of 'shape_or_null.dart';


//class reflection

class ShapeOrNullReflection extends ClassReflection<ShapeOrNull> {
  static ShapeOrNullReflection instanceGetter() => instance;
  static const instance = ShapeOrNullReflection._(
    modelName: r'ShapeOrNull',
    className: r'ShapeOrNull',
    discriminatorKey: r'shapeType',
    discriminatorImplicitMappings: const {
      r'Quadrilateral': Quadrilateral.$reflection,
      r'Triangle': Triangle.$reflection,
    },
    discriminatorMappings: const {
      r'Quadrilateral': Quadrilateral.$reflection,
      r'Triangle': Triangle.$reflection,
    },
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: TriangleReflection.instance,
    ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: QuadrilateralReflection.instance,
    ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ShapeOrNull, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ShapeOrNullReflection._({
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
  List<PropertyReflection<ShapeOrNull, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<ShapeOrNull, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<ShapeOrNull, 
            Triangle
> oneOf0Part;
  
  final OneOfReflection<ShapeOrNull, 
            Quadrilateral
> oneOf1Part;
  
  @override
  List<PartReflection<ShapeOrNull, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ShapeOrNull, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ShapeOrNull, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<ShapeOrNull, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ShapeOrNull.canDeserialize(src);
  @override
  ShapeOrNull Function(Object? src) get deserializeFunction =>
      (src) => ShapeOrNull.deserialize(src);

  @override
  Object? Function(ShapeOrNull src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ShapeOrNull.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ShapeOrNull example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ShapeOrNull(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    Triangle.$reflection.example(discriminators: discriminators)
    


);
      }(),
      
    );
  }
}

class ShapeOrNullXmlReflection {
    const ShapeOrNullXmlReflection();
}

