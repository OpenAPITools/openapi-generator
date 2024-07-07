// Model reflection

part of 'nullable_shape.dart';


//class reflection

class NullableShapeReflection extends ClassReflection<NullableShape> {
  static NullableShapeReflection instanceGetter() => instance;
  static const instance = NullableShapeReflection._(
    modelName: r'NullableShape',
    className: r'NullableShape',
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
      itemsReflection: ItemsReflection<NullableShape, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const NullableShapeReflection._({
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
  List<PropertyReflection<NullableShape, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<NullableShape, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<NullableShape, 
            Triangle
> oneOf0Part;
  
  final OneOfReflection<NullableShape, 
            Quadrilateral
> oneOf1Part;
  
  @override
  List<PartReflection<NullableShape, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<NullableShape, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<NullableShape, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<NullableShape, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NullableShape.canDeserialize(src);
  @override
  NullableShape Function(Object? src) get deserializeFunction =>
      (src) => NullableShape.deserialize(src);

  @override
  Object? Function(NullableShape src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of NullableShape.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  NullableShape example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return NullableShape(
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

class NullableShapeXmlReflection {
    const NullableShapeXmlReflection();
}

