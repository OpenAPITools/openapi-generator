// Model reflection

part of 'triangle.dart';


//class reflection

class TriangleReflection extends ClassReflection<Triangle> {
  static TriangleReflection instanceGetter() => instance;
  static const instance = TriangleReflection._(
    modelName: r'Triangle',
    className: r'Triangle',
    triangleTypePart: PropertyReflection<Triangle, 
            String

>(
      dartName: r'triangleType',
      nullable: false,
      required: true,
      oasName: r'triangleType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      getter: _triangleTypeGetter,
      setter: _triangleTypeSetter,
    ),
    discriminatorKey: r'triangleType',
    discriminatorImplicitMappings: const {
      r'EquilateralTriangle': EquilateralTriangleReflection.instance,
      r'IsoscelesTriangle': IsoscelesTriangleReflection.instance,
      r'ScaleneTriangle': ScaleneTriangleReflection.instance,
    },
    discriminatorMappings: const {
      r'EquilateralTriangle': EquilateralTriangleReflection.instance,
      r'IsoscelesTriangle': IsoscelesTriangleReflection.instance,
      r'ScaleneTriangle': ScaleneTriangleReflection.instance,
    },
    
    
    oneOf0Part: TriangleOneOf0(
      parentReflectionGetter: instanceGetter,
      classReflection: EquilateralTriangleReflection.instance,
    ),
    
    oneOf1Part: TriangleOneOf1(
      parentReflectionGetter: instanceGetter,
      classReflection: IsoscelesTriangleReflection.instance,
    ),
    
    oneOf2Part: TriangleOneOf2(
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
    required this.triangleTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.oneOf2Part,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Triangle, 
            String

> triangleTypePart;
  static 
            String

 _triangleTypeGetter(Triangle parent) {
    return parent.triangleType;
  }
  static void _triangleTypeSetter(Triangle parent, 
            String

 value) {
    parent.triangleType = value;
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
  List<PropertyReflection<Triangle, dynamic>> get properties => [
    triangleTypePart,
  ];

  final AdditionalPropertiesReflection<Triangle, Object

?> additionalPropertiesPart;

  
  
  final TriangleOneOf0 oneOf0Part;
  
  final TriangleOneOf1 oneOf1Part;
  
  final TriangleOneOf2 oneOf2Part;
  
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
  Triangle example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Triangle(
      triangleType: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[triangleTypePart.oasName]?.key.key;
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
    
    exampleResult.oneOf2 = oneOf2Part.example(discriminators: actualDiscriminators, discriminatorExampleResults: discriminatorExampleResults);
    
    return exampleResult;
  }
}


class TriangleOneOf0 extends OneOfReflection<Triangle, 
            EquilateralTriangle
> {
  const TriangleOneOf0({
    super.classReflection,
    required TriangleReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            EquilateralTriangle
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
            
            


    EquilateralTriangleReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class TriangleOneOf1 extends OneOfReflection<Triangle, 
            IsoscelesTriangle
> {
  const TriangleOneOf1({
    super.classReflection,
    required TriangleReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            IsoscelesTriangle
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
            
            


    IsoscelesTriangleReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class TriangleOneOf2 extends OneOfReflection<Triangle, 
            ScaleneTriangle
> {
  const TriangleOneOf2({
    super.classReflection,
    required TriangleReflection Function() super.parentReflectionGetter,
    super.itemsReflection,
  });

  UndefinedWrapper<
            ScaleneTriangle
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
            
            


    ScaleneTriangleReflection.instance.example(discriminators: discriminators, discriminatorExampleResults: discriminatorExampleResults)
    
);
  }
}

class TriangleXmlReflection {
    const TriangleXmlReflection();
}

