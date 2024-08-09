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
      r'Quadrilateral': QuadrilateralReflection.instance,
      r'Triangle': TriangleReflection.instance,
    },
    discriminatorMappings: const {
      r'Quadrilateral': QuadrilateralReflection.instance,
      r'Triangle': TriangleReflection.instance,
    },
    
    
    oneOf0Part: ShapeOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: TriangleReflection.instance,
    ),
    
    oneOf1Part: ShapeOneOf1(
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

  
  
  final ShapeOneOf0 oneOf0Part;
  
  final ShapeOneOf1 oneOf1Part;
  
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
  Shape example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Shape(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class ShapeOneOf0 extends OneOfReflection<Shape, 
            Triangle
> {
  const ShapeOneOf0({
    super.classReflection,
    required ShapeReflection Function() super.parentReflectionGetter,
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

class ShapeOneOf1 extends OneOfReflection<Shape, 
            Quadrilateral
> {
  const ShapeOneOf1({
    super.classReflection,
    required ShapeReflection Function() super.parentReflectionGetter,
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

class ShapeXmlReflection {
    const ShapeXmlReflection();
}

