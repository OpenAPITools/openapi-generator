// Model reflection

part of 'simple_quadrilateral.dart';


//class reflection

class SimpleQuadrilateralReflection extends ClassReflection<SimpleQuadrilateral> {
  static SimpleQuadrilateralReflection instanceGetter() => instance;
  static const instance = SimpleQuadrilateralReflection._(
    modelName: r'SimpleQuadrilateral',
    className: r'SimpleQuadrilateral',
    quadrilateralTypePart: PropertyReflection<SimpleQuadrilateral, 
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
    shapeTypePart: PropertyReflection<SimpleQuadrilateral, 
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
      itemsReflection: ItemsReflection<SimpleQuadrilateral, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const SimpleQuadrilateralReflection._({
    required this.modelName,
    required this.className,
    required this.quadrilateralTypePart,
    required this.shapeTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<SimpleQuadrilateral, 
            String

> quadrilateralTypePart;
  static 
            String

 _quadrilateralTypeGetter(SimpleQuadrilateral parent) {
    return parent.quadrilateralType;
  }
  static void _quadrilateralTypeSetter(SimpleQuadrilateral parent, 
            String

 value) {
    parent.quadrilateralType = value;
  }
  final PropertyReflection<SimpleQuadrilateral, 
            String

> shapeTypePart;
  static 
            String

 _shapeTypeGetter(SimpleQuadrilateral parent) {
    return parent.shapeType;
  }
  static void _shapeTypeSetter(SimpleQuadrilateral parent, 
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
  List<PropertyReflection<SimpleQuadrilateral, dynamic>> get properties => [
    quadrilateralTypePart,
shapeTypePart,
  ];

  final AdditionalPropertiesReflection<SimpleQuadrilateral, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<SimpleQuadrilateral, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<SimpleQuadrilateral, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<SimpleQuadrilateral, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<SimpleQuadrilateral, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => SimpleQuadrilateral.canDeserialize(src);
  @override
  SimpleQuadrilateral Function(Object? src) get deserializeFunction =>
      (src) => SimpleQuadrilateral.deserialize(src);

  @override
  Object? Function(SimpleQuadrilateral src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of SimpleQuadrilateral.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  SimpleQuadrilateral example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = SimpleQuadrilateral(
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


class SimpleQuadrilateralXmlReflection {
    const SimpleQuadrilateralXmlReflection();
}

