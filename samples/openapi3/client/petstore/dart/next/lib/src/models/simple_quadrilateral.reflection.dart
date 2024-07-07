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
  SimpleQuadrilateral example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return SimpleQuadrilateral(
      quadrilateralType: () {
        PartReflection? _partReflection = _reflection.quadrilateralTypePart;
        
        final disc = discriminators[r'quadrilateralType'];
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
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class SimpleQuadrilateralXmlReflection {
    const SimpleQuadrilateralXmlReflection();
}

