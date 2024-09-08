// Model reflection

part of 'shape_interface.dart';


//class reflection

class ShapeInterfaceReflection extends ModelReflection<ShapeInterface> {
  static ShapeInterfaceReflection instanceGetter() => instance;
  static const instance = ShapeInterfaceReflection._(
    modelName: r'ShapeInterface',
    className: r'ShapeInterface',
    xml: XmlReflection(
),
    shapeTypePart: PropertyReflection<ShapeInterface, 
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
  const ShapeInterfaceReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.shapeTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ShapeInterface, 
            String
> shapeTypePart;
  static 
            String
 _shapeTypeGetter(ShapeInterface parent) {
    return parent.shapeType;
  }
  static void _shapeTypeSetter(ShapeInterface parent, 
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
  List<PropertyReflection<ShapeInterface, dynamic>> get properties => [
    shapeTypePart,
  ];

  @override
  final AdditionalPropertiesPart<ShapeInterface, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(ShapeInterface instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(ShapeInterface instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<ShapeInterface, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  ShapeInterface empty() {
    return ShapeInterface(
      shapeType: shapeTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ShapeInterfaceReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


