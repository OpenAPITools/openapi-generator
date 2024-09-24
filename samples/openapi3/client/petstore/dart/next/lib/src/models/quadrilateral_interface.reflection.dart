// Model reflection

part of 'quadrilateral_interface.dart';


//class reflection

class QuadrilateralInterfaceReflection extends ModelReflection<QuadrilateralInterface> {
  static QuadrilateralInterfaceReflection instanceGetter() => instance;
  static const instance = QuadrilateralInterfaceReflection._(
    modelName: r'QuadrilateralInterface',
    className: r'QuadrilateralInterface',
    xml: XmlReflection(
),
    quadrilateralTypePart: PropertyReflection<QuadrilateralInterface, 
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
  const QuadrilateralInterfaceReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.quadrilateralTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<QuadrilateralInterface, 
            String
> quadrilateralTypePart;
  static 
            String
 _quadrilateralTypeGetter(QuadrilateralInterface parent) {
    return parent.quadrilateralType;
  }
  static void _quadrilateralTypeSetter(QuadrilateralInterface parent, 
            String
 value) {
    parent.quadrilateralType = value;
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
  List<PropertyReflection<QuadrilateralInterface, dynamic>> get properties => [
    quadrilateralTypePart,
  ];

  @override
  final AdditionalPropertiesPart<QuadrilateralInterface, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(QuadrilateralInterface instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(QuadrilateralInterface instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<QuadrilateralInterface, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  QuadrilateralInterface empty() {
    return QuadrilateralInterface(
      quadrilateralType: quadrilateralTypePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is QuadrilateralInterfaceReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


