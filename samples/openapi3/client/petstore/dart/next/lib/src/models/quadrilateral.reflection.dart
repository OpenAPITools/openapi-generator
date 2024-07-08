// Model reflection

part of 'quadrilateral.dart';


//class reflection

class QuadrilateralReflection extends ClassReflection<Quadrilateral> {
  static QuadrilateralReflection instanceGetter() => instance;
  static const instance = QuadrilateralReflection._(
    modelName: r'Quadrilateral',
    className: r'Quadrilateral',
    quadrilateralTypePart: PropertyReflection<Quadrilateral, 
            String

>(
      dartName: r'quadrilateralType',
      nullable: false,
      required: true,
      oasName: r'quadrilateralType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      getter: _quadrilateralTypeGetter,
      setter: _quadrilateralTypeSetter,
    ),
    discriminatorKey: r'quadrilateralType',
    discriminatorImplicitMappings: const {
      r'ComplexQuadrilateral': ComplexQuadrilateralReflection.instance,
      r'SimpleQuadrilateral': SimpleQuadrilateralReflection.instance,
    },
    discriminatorMappings: const {
      r'ComplexQuadrilateral': ComplexQuadrilateralReflection.instance,
      r'SimpleQuadrilateral': SimpleQuadrilateralReflection.instance,
    },
    
    
    oneOf0Part: QuadrilateralOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: SimpleQuadrilateralReflection.instance,
    ),
    
    oneOf1Part: QuadrilateralOneOf1(
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
    required this.quadrilateralTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Quadrilateral, 
            String

> quadrilateralTypePart;
  static 
            String

 _quadrilateralTypeGetter(Quadrilateral parent) {
    return parent.quadrilateralType;
  }
  static void _quadrilateralTypeSetter(Quadrilateral parent, 
            String

 value) {
    parent.quadrilateralType = value;
  }



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
    quadrilateralTypePart,
  ];

  final AdditionalPropertiesReflection<Quadrilateral, Object

?> additionalPropertiesPart;

  
  
  final QuadrilateralOneOf0 oneOf0Part;
  
  final QuadrilateralOneOf1 oneOf1Part;
  
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
  Quadrilateral example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Quadrilateral(
      quadrilateralType: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[quadrilateralTypePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return result;
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    exampleResult.oneOf0 = oneOf0Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    exampleResult.oneOf1 = oneOf1Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class QuadrilateralOneOf0 extends OneOfReflection<Quadrilateral, 
            SimpleQuadrilateral
> {
  const QuadrilateralOneOf0({
    super.classReflection,
    required QuadrilateralReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            SimpleQuadrilateral
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
            
            


    SimpleQuadrilateralReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class QuadrilateralOneOf1 extends OneOfReflection<Quadrilateral, 
            ComplexQuadrilateral
> {
  const QuadrilateralOneOf1({
    super.classReflection,
    required QuadrilateralReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            ComplexQuadrilateral
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
            
            


    ComplexQuadrilateralReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class QuadrilateralXmlReflection {
    const QuadrilateralXmlReflection();
}

