// Model reflection

part of 'equilateral_triangle.dart';


//class reflection

class EquilateralTriangleReflection extends ModelReflection<EquilateralTriangle> {
  static EquilateralTriangleReflection instanceGetter() => instance;
  static const instance = EquilateralTriangleReflection._(
    modelName: r'EquilateralTriangle',
    className: r'EquilateralTriangle',
    xml: XmlReflection(
),
    shapeTypePart: PropertyReflection<EquilateralTriangle, 
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
    triangleTypePart: PropertyReflection<EquilateralTriangle, 
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
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_triangleTypeGetter),
      setter: FunctionWrapper2(_triangleTypeSetter),
      reflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
,
    ),
    allOfShapeInterfacePart: AllOfReflection(
      parentReflectionGetter: instanceGetter,
      reflection: ShapeInterfaceReflection.instance,
    ),
allOfTriangleInterfacePart: AllOfReflection(
      parentReflectionGetter: instanceGetter,
      reflection: TriangleInterfaceReflection.instance,
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
  const EquilateralTriangleReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.shapeTypePart,
    required this.triangleTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
    required this.allOfShapeInterfacePart,
required this.allOfTriangleInterfacePart,
    
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<EquilateralTriangle, 
            String
> shapeTypePart;
  static 
            String
 _shapeTypeGetter(EquilateralTriangle parent) {
    return parent.shapeType;
  }
  static void _shapeTypeSetter(EquilateralTriangle parent, 
            String
 value) {
    parent.shapeType = value;
  }

  final PropertyReflection<EquilateralTriangle, 
            String
> triangleTypePart;
  static 
            String
 _triangleTypeGetter(EquilateralTriangle parent) {
    return parent.triangleType;
  }
  static void _triangleTypeSetter(EquilateralTriangle parent, 
            String
 value) {
    parent.triangleType = value;
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
  List<PropertyReflection<EquilateralTriangle, dynamic>> get properties => [
    shapeTypePart,
triangleTypePart,
  ];

  @override
  final AdditionalPropertiesPart<EquilateralTriangle, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(EquilateralTriangle instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(EquilateralTriangle instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  final AllOfReflection<EquilateralTriangle, ShapeInterfaceMixin> allOfShapeInterfacePart;
final AllOfReflection<EquilateralTriangle, TriangleInterfaceMixin> allOfTriangleInterfacePart;

  

  @override
  List<AllOfReflection<EquilateralTriangle, Object>> get allOfs => [
    allOfShapeInterfacePart,allOfTriangleInterfacePart,
  ];

  @override
  List<OneOfReflection<EquilateralTriangle, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<EquilateralTriangle, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  EquilateralTriangle empty() {
    return EquilateralTriangle(
      shapeType: shapeTypePart.reflection.emptyFunction(),
      triangleType: triangleTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is EquilateralTriangleReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


