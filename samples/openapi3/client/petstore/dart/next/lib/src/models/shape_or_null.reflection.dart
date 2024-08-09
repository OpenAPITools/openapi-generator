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
      r'Quadrilateral': QuadrilateralReflection.instance,
      r'Triangle': TriangleReflection.instance,
    },
    discriminatorMappings: const {
      r'Quadrilateral': QuadrilateralReflection.instance,
      r'Triangle': TriangleReflection.instance,
    },
    
    
    oneOf0Part: ShapeOrNullOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: TriangleReflection.instance,
    ),
    
    oneOf1Part: ShapeOrNullOneOf1(
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

  
  
  final ShapeOrNullOneOf0 oneOf0Part;
  
  final ShapeOrNullOneOf1 oneOf1Part;
  
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
  ShapeOrNull example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ShapeOrNull(
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class ShapeOrNullOneOf0 extends OneOfReflection<ShapeOrNull, 
            Triangle
> {
  const ShapeOrNullOneOf0({
    super.classReflection,
    required ShapeOrNullReflection Function() super.parentReflectionGetter,
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

class ShapeOrNullOneOf1 extends OneOfReflection<ShapeOrNull, 
            Quadrilateral
> {
  const ShapeOrNullOneOf1({
    super.classReflection,
    required ShapeOrNullReflection Function() super.parentReflectionGetter,
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

class ShapeOrNullXmlReflection {
    const ShapeOrNullXmlReflection();
}

