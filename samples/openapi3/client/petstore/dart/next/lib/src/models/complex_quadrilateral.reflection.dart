// Model reflection

part of 'complex_quadrilateral.dart';


//class reflection

class ComplexQuadrilateralReflection extends ModelReflection<ComplexQuadrilateral> {
  static ComplexQuadrilateralReflection instanceGetter() => instance;
  static const instance = ComplexQuadrilateralReflection._(
    modelName: r'ComplexQuadrilateral',
    className: r'ComplexQuadrilateral',
    xml: XmlReflection(
),
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_quadrilateralTypeGetter),
      setter: FunctionWrapper2(_quadrilateralTypeSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_shapeTypeGetter),
      setter: FunctionWrapper2(_shapeTypeSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    allOfQuadrilateralInterfacePart: AllOfReflection(
      parentReflectionGetter: instanceGetter,
      reflection: QuadrilateralInterfaceReflection.instance,
    ),
allOfShapeInterfacePart: AllOfReflection(
      parentReflectionGetter: instanceGetter,
      reflection: ShapeInterfaceReflection.instance,
    ),

    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(ObjectReflection()
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const ComplexQuadrilateralReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.quadrilateralTypePart,
    required this.shapeTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
    required this.allOfQuadrilateralInterfacePart,
required this.allOfShapeInterfacePart,
    
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
  final Map<String, ModelReflection> discriminatorMappings;
  @override
  final Map<String, ModelReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;
  @override
  final XmlReflection xml;

  @override
  List<PropertyReflection<ComplexQuadrilateral, dynamic>> get properties => [
    quadrilateralTypePart,
shapeTypePart,
  ];

  @override
  final AdditionalPropertiesPart<ComplexQuadrilateral, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ComplexQuadrilateral instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ComplexQuadrilateral instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  final AllOfReflection<ComplexQuadrilateral, QuadrilateralInterfaceMixin> allOfQuadrilateralInterfacePart;
final AllOfReflection<ComplexQuadrilateral, ShapeInterfaceMixin> allOfShapeInterfacePart;

  

  @override
  List<AllOfReflection<ComplexQuadrilateral, Object>> get allOfs => [
    allOfQuadrilateralInterfacePart,allOfShapeInterfacePart,
  ];

  @override
  List<OneOfReflection<ComplexQuadrilateral, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ComplexQuadrilateral, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ComplexQuadrilateral empty() {
    return ComplexQuadrilateral(
      quadrilateralType: quadrilateralTypePart.reflection.emptyFunction(),
      shapeType: shapeTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ComplexQuadrilateralReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


