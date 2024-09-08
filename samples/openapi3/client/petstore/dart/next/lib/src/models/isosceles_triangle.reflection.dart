// Model reflection

part of 'isosceles_triangle.dart';


//class reflection

class IsoscelesTriangleReflection extends ModelReflection<IsoscelesTriangle> {
  static IsoscelesTriangleReflection instanceGetter() => instance;
  static const instance = IsoscelesTriangleReflection._(
    modelName: r'IsoscelesTriangle',
    className: r'IsoscelesTriangle',
    xml: XmlReflection(
),
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

    
  );
  const IsoscelesTriangleReflection._({
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
  List<PropertyReflection<IsoscelesTriangle, dynamic>> get properties => [
    shapeTypePart,
triangleTypePart,
  ];


  final AllOfReflection<IsoscelesTriangle, ShapeInterfaceMixin> allOfShapeInterfacePart;
final AllOfReflection<IsoscelesTriangle, TriangleInterfaceMixin> allOfTriangleInterfacePart;

  

  @override
  List<AllOfReflection<IsoscelesTriangle, Object>> get allOfs => [
    allOfShapeInterfacePart,allOfTriangleInterfacePart,
  ];

  @override
  List<OneOfReflection<IsoscelesTriangle, Object>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<IsoscelesTriangle, Object>> get anyOfs => [
    
  ];


  /// Creates an empty instance used as a starting point for deserialization.
  @override
  IsoscelesTriangle empty() {
    return IsoscelesTriangle(
      shapeType: shapeTypePart.reflection.emptyFunction(),
      triangleType: triangleTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is IsoscelesTriangleReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


