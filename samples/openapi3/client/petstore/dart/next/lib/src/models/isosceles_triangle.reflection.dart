// Model reflection

part of 'isosceles_triangle.dart';


//class reflection

class IsoscelesTriangleReflection extends ClassReflection<IsoscelesTriangle> {
  static IsoscelesTriangleReflection instanceGetter() => instance;
  static const instance = IsoscelesTriangleReflection._(
    modelName: r'IsoscelesTriangle',
    className: r'IsoscelesTriangle',
    shapeTypePart: PropertyReflection<IsoscelesTriangle, 
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
    triangleTypePart: PropertyReflection<IsoscelesTriangle, 
            String

>(
      dartName: r'triangleType',
      nullable: false,
      required: true,
      oasName: r'triangleType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _triangleTypeGetter,
      setter: _triangleTypeSetter,
    ),
    
    
  );
  const IsoscelesTriangleReflection._({
    required this.modelName,
    required this.className,
    required this.shapeTypePart,
    required this.triangleTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
  });

  final PropertyReflection<IsoscelesTriangle, 
            String

> shapeTypePart;
  static 
            String

 _shapeTypeGetter(IsoscelesTriangle parent) {
    return parent.shapeType;
  }
  static void _shapeTypeSetter(IsoscelesTriangle parent, 
            String

 value) {
    parent.shapeType = value;
  }
  final PropertyReflection<IsoscelesTriangle, 
            String

> triangleTypePart;
  static 
            String

 _triangleTypeGetter(IsoscelesTriangle parent) {
    return parent.triangleType;
  }
  static void _triangleTypeSetter(IsoscelesTriangle parent, 
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
  List<PropertyReflection<IsoscelesTriangle, dynamic>> get properties => [
    shapeTypePart,
triangleTypePart,
  ];

  
  
  
  @override
  List<PartReflection<IsoscelesTriangle, dynamic>> get parts => [
    ...super.parts,
      ];
  @override
  List<AllOfReflection<IsoscelesTriangle, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<IsoscelesTriangle, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<IsoscelesTriangle, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => IsoscelesTriangle.canDeserialize(src);
  @override
  IsoscelesTriangle Function(Object? src) get deserializeFunction =>
      (src) => IsoscelesTriangle.deserialize(src);

  @override
  Object? Function(IsoscelesTriangle src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of IsoscelesTriangle.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  IsoscelesTriangle example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = IsoscelesTriangle(
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
      
    );
    
    return exampleResult;
  }
}


class IsoscelesTriangleXmlReflection {
    const IsoscelesTriangleXmlReflection();
}

