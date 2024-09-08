// Model reflection

part of 'simple_quadrilateral.dart';


//class reflection

class SimpleQuadrilateralReflection extends ModelReflection<SimpleQuadrilateral> {
  static SimpleQuadrilateralReflection instanceGetter() => instance;
  static const instance = SimpleQuadrilateralReflection._(
    modelName: r'SimpleQuadrilateral',
    className: r'SimpleQuadrilateral',
    xml: XmlReflection(
),
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
  const SimpleQuadrilateralReflection._({
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
  List<PropertyReflection<SimpleQuadrilateral, dynamic>> get properties => [
    quadrilateralTypePart,
shapeTypePart,
  ];

  @override
  final AdditionalPropertiesPart<SimpleQuadrilateral, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(SimpleQuadrilateral instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(SimpleQuadrilateral instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  final AllOfReflection<SimpleQuadrilateral, QuadrilateralInterfaceMixin> allOfQuadrilateralInterfacePart;
final AllOfReflection<SimpleQuadrilateral, ShapeInterfaceMixin> allOfShapeInterfacePart;

  

  @override
  List<AllOfReflection<SimpleQuadrilateral, Object>> get allOfs => [
    allOfQuadrilateralInterfacePart,allOfShapeInterfacePart,
  ];

  @override
  List<OneOfReflection<SimpleQuadrilateral, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<SimpleQuadrilateral, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  SimpleQuadrilateral empty() {
    return SimpleQuadrilateral(
      quadrilateralType: quadrilateralTypePart.reflection.emptyFunction(),
      shapeType: shapeTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is SimpleQuadrilateralReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


