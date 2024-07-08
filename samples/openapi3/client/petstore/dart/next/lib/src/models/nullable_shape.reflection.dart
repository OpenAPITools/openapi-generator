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
      r'Quadrilateral': QuadrilateralReflection.instance,
      r'Triangle': TriangleReflection.instance,
    },
    discriminatorMappings: const {
      r'Quadrilateral': QuadrilateralReflection.instance,
      r'Triangle': TriangleReflection.instance,
    },
    
    
    oneOf0Part: NullableShapeOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: TriangleReflection.instance,
    ),
    
    oneOf1Part: NullableShapeOneOf1(
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

  
  
  final NullableShapeOneOf0 oneOf0Part;
  
  final NullableShapeOneOf1 oneOf1Part;
  
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
  NullableShape example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = NullableShape(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class NullableShapeOneOf0 extends OneOfReflection<NullableShape, 
            Triangle
> {
  const NullableShapeOneOf0({
    super.classReflection,
    required NullableShapeReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Triangle
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      // An example SHOULD be generated
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return UndefinedWrapper(
            
            


    TriangleReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class NullableShapeOneOf1 extends OneOfReflection<NullableShape, 
            Quadrilateral
> {
  const NullableShapeOneOf1({
    super.classReflection,
    required NullableShapeReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            Quadrilateral
> example({required AggregatedDiscriminatorsResult discriminators, required Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>> discriminatorExampleResults}) {
    if (discriminatorExampleResults.isEmpty) {
      //return undefined for non-first oneOfs.
      return UndefinedWrapper.undefined();
    } else {
      // if this reflection wasn't a result of any property, don't generate an example.

      if (!discriminatorExampleResults.values
          .any((e) => e.value == classReflection)) {
        // if there are no discriminator examples targetting the current class:
        return UndefinedWrapper.undefined();
      } else {
        // An example SHOULD be generated
      }
    }
    return UndefinedWrapper(
            
            


    QuadrilateralReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class NullableShapeXmlReflection {
    const NullableShapeXmlReflection();
}

