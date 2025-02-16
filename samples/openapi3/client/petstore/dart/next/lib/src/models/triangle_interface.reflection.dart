// Model reflection

part of 'triangle_interface.dart';


//class reflection

class TriangleInterfaceReflection extends ModelReflection<TriangleInterface> {
  static TriangleInterfaceReflection instanceGetter() => instance;
  static const instance = TriangleInterfaceReflection._(
    modelName: r'TriangleInterface',
    className: r'TriangleInterface',
    xml: XmlReflection(
),
    triangleTypePart: PropertyReflection<TriangleInterface, 
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
  const TriangleInterfaceReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.triangleTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<TriangleInterface, 
            String
> triangleTypePart;
  static 
            String
 _triangleTypeGetter(TriangleInterface parent) {
    return parent.triangleType;
  }
  static void _triangleTypeSetter(TriangleInterface parent, 
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
  List<PropertyReflection<TriangleInterface, dynamic>> get properties => [
    triangleTypePart,
  ];

  @override
  final AdditionalPropertiesPart<TriangleInterface, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(TriangleInterface instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(TriangleInterface instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<TriangleInterface, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  TriangleInterface empty() {
    return TriangleInterface(
      triangleType: triangleTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is TriangleInterfaceReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


