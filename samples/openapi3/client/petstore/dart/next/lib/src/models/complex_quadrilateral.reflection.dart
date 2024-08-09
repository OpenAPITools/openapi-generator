// Model reflection

part of 'complex_quadrilateral.dart';


//class reflection

class ComplexQuadrilateralReflection extends ClassReflection<ComplexQuadrilateral> {
  static ComplexQuadrilateralReflection instanceGetter() => instance;
  static const instance = ComplexQuadrilateralReflection._(
    modelName: r'ComplexQuadrilateral',
    className: r'ComplexQuadrilateral',
    quadrilateralTypePart: PropertyReflection<ComplexQuadrilateral, 
            String

>(
      dartName: r'quadrilateralType',
      nullable: false,
      required: true,
      oasName: r'quadrilateralType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _quadrilateralTypeGetter,
      setter: _quadrilateralTypeSetter,
    ),
    shapeTypePart: PropertyReflection<ComplexQuadrilateral, 
            String

>(
      dartName: r'shapeType',
      nullable: false,
      required: true,
      oasName: r'shapeType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _shapeTypeGetter,
      setter: _shapeTypeSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ComplexQuadrilateral, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ComplexQuadrilateralReflection._({
    required this.modelName,
    required this.className,
    required this.quadrilateralTypePart,
    required this.shapeTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ComplexQuadrilateral, 
            String

> quadrilateralTypePart;
  static 
            String

 _quadrilateralTypeGetter(ComplexQuadrilateral parent) {
    return parent.quadrilateralType;
  }
  static void _quadrilateralTypeSetter(ComplexQuadrilateral parent, 
            String

 value) {
    parent.quadrilateralType = value;
  }
  final PropertyReflection<ComplexQuadrilateral, 
            String

> shapeTypePart;
  static 
            String

 _shapeTypeGetter(ComplexQuadrilateral parent) {
    return parent.shapeType;
  }
  static void _shapeTypeSetter(ComplexQuadrilateral parent, 
            String

 value) {
    parent.shapeType = value;
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
  List<PropertyReflection<ComplexQuadrilateral, dynamic>> get properties => [
    quadrilateralTypePart,
shapeTypePart,
  ];

  final AdditionalPropertiesReflection<ComplexQuadrilateral, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ComplexQuadrilateral, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ComplexQuadrilateral, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ComplexQuadrilateral, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ComplexQuadrilateral, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ComplexQuadrilateral.canDeserialize(src);
  @override
  ComplexQuadrilateral Function(Object? src) get deserializeFunction =>
      (src) => ComplexQuadrilateral.deserialize(src);

  @override
  Object? Function(ComplexQuadrilateral src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ComplexQuadrilateral.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ComplexQuadrilateral example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ComplexQuadrilateral(
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
      shapeType: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[shapeTypePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return result;
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class ComplexQuadrilateralXmlReflection {
    const ComplexQuadrilateralXmlReflection();
}

