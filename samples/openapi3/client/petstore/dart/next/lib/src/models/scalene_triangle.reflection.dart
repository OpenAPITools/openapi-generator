// Model reflection

part of 'scalene_triangle.dart';


//class reflection

class ScaleneTriangleReflection extends ModelReflection<ScaleneTriangle> {
  static ScaleneTriangleReflection instanceGetter() => instance;
  static const instance = ScaleneTriangleReflection._(
    modelName: r'ScaleneTriangle',
    className: r'ScaleneTriangle',
    xml: XmlReflection(
),
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
  const ScaleneTriangleReflection._({
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
  List<PropertyReflection<ScaleneTriangle, dynamic>> get properties => [
    shapeTypePart,
triangleTypePart,
  ];

  @override
  final AdditionalPropertiesPart<ScaleneTriangle, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ScaleneTriangle instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ScaleneTriangle instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  final AllOfReflection<ScaleneTriangle, ShapeInterfaceMixin> allOfShapeInterfacePart;
final AllOfReflection<ScaleneTriangle, TriangleInterfaceMixin> allOfTriangleInterfacePart;

  

  @override
  List<AllOfReflection<ScaleneTriangle, Object>> get allOfs => [
    allOfShapeInterfacePart,allOfTriangleInterfacePart,
  ];

  @override
  List<OneOfReflection<ScaleneTriangle, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ScaleneTriangle, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ScaleneTriangle empty() {
    return ScaleneTriangle(
      shapeType: shapeTypePart.reflection.emptyFunction(),
      triangleType: triangleTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ScaleneTriangleReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


