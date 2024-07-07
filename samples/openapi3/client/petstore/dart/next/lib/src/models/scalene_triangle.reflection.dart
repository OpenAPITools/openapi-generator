// Model reflection

part of 'scalene_triangle.dart';


//class reflection

class ScaleneTriangleReflection extends ClassReflection<ScaleneTriangle> {
  static ScaleneTriangleReflection instanceGetter() => instance;
  static const instance = ScaleneTriangleReflection._(
    modelName: r'ScaleneTriangle',
    className: r'ScaleneTriangle',
    shapeTypePart: PropertyReflection<ScaleneTriangle, 
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
    triangleTypePart: PropertyReflection<ScaleneTriangle, 
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
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ScaleneTriangle, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ScaleneTriangleReflection._({
    required this.modelName,
    required this.className,
    required this.shapeTypePart,
    required this.triangleTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ScaleneTriangle, 
            String
> shapeTypePart;
  static 
            String
 _shapeTypeGetter(ScaleneTriangle parent) {
    return parent.shapeType;
  }
  static void _shapeTypeSetter(ScaleneTriangle parent, 
            String
 value) {
    parent.shapeType = value;
  }
  final PropertyReflection<ScaleneTriangle, 
            String
> triangleTypePart;
  static 
            String
 _triangleTypeGetter(ScaleneTriangle parent) {
    return parent.triangleType;
  }
  static void _triangleTypeSetter(ScaleneTriangle parent, 
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
  List<PropertyReflection<ScaleneTriangle, dynamic>> get properties => [
    shapeTypePart,
triangleTypePart,
  ];

  final AdditionalPropertiesReflection<ScaleneTriangle, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ScaleneTriangle, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ScaleneTriangle, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ScaleneTriangle, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ScaleneTriangle, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ScaleneTriangle.canDeserialize(src);
  @override
  ScaleneTriangle Function(Object? src) get deserializeFunction =>
      (src) => ScaleneTriangle.deserialize(src);

  @override
  Object? Function(ScaleneTriangle src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ScaleneTriangle.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ScaleneTriangle example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ScaleneTriangle(
      shapeType: () {
        PartReflection? _partReflection = _reflection.shapeTypePart;
        
        final disc = discriminators[r'shapeType'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      triangleType: () {
        PartReflection? _partReflection = _reflection.triangleTypePart;
        
        final disc = discriminators[r'triangleType'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class ScaleneTriangleXmlReflection {
    const ScaleneTriangleXmlReflection();
}

