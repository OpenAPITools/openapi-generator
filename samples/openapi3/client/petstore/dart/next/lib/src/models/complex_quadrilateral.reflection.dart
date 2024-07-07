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
  ComplexQuadrilateral example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ComplexQuadrilateral(
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

class ComplexQuadrilateralXmlReflection {
    const ComplexQuadrilateralXmlReflection();
}

