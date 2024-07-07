// Model reflection

part of 'shape.dart';


//class reflection

class ShapeReflection extends ClassReflection<Shape> {
  static ShapeReflection instanceGetter() => instance;
  static const instance = ShapeReflection._(
    modelName: r'Shape',
    className: r'Shape',
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
      itemsReflection: ItemsReflection<Shape, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ShapeReflection._({
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
  List<PropertyReflection<Shape, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<Shape, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<Shape, 
            Triangle
> oneOf0Part;
  
  final OneOfReflection<Shape, 
            Quadrilateral
> oneOf1Part;
  
  @override
  List<PartReflection<Shape, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Shape, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Shape, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Shape, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Shape.canDeserialize(src);
  @override
  Shape Function(Object? src) get deserializeFunction =>
      (src) => Shape.deserialize(src);

  @override
  Object? Function(Shape src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Shape.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Shape example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Shape(
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

class ShapeXmlReflection {
    const ShapeXmlReflection();
}

