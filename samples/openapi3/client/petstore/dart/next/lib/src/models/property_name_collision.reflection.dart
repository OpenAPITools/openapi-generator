// Model reflection

part of 'property_name_collision.dart';


//class reflection

class PropertyNameCollisionReflection extends ModelReflection<PropertyNameCollision> {
  static PropertyNameCollisionReflection instanceGetter() => instance;
  static const instance = PropertyNameCollisionReflection._(
    modelName: r'PropertyNameCollision',
    className: r'PropertyNameCollision',
    xml: XmlReflection(
),
    $typePart: PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String
>>(
      dartName: r'$type',
      nullable: false,
      required: false,
      oasName: r'_type',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_$typeGetter),
      setter: FunctionWrapper2(_$typeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    typePart: PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String
>>(
      dartName: r'type',
      nullable: false,
      required: false,
      oasName: r'type',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_typeGetter),
      setter: FunctionWrapper2(_typeSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    type$Part: PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String
>>(
      dartName: r'type$',
      nullable: false,
      required: false,
      oasName: r'type_',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_type$Getter),
      setter: FunctionWrapper2(_type$Setter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
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
  const PropertyNameCollisionReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.$typePart,
    required this.typePart,
    required this.type$Part,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String
>> $typePart;
  static UndefinedWrapper<
            String
> _$typeGetter(PropertyNameCollision parent) {
    return parent.$type;
  }
  static void _$typeSetter(PropertyNameCollision parent, UndefinedWrapper<
            String
> value) {
    parent.$type = value;
  }

  final PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String
>> typePart;
  static UndefinedWrapper<
            String
> _typeGetter(PropertyNameCollision parent) {
    return parent.type;
  }
  static void _typeSetter(PropertyNameCollision parent, UndefinedWrapper<
            String
> value) {
    parent.type = value;
  }

  final PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String
>> type$Part;
  static UndefinedWrapper<
            String
> _type$Getter(PropertyNameCollision parent) {
    return parent.type$;
  }
  static void _type$Setter(PropertyNameCollision parent, UndefinedWrapper<
            String
> value) {
    parent.type$ = value;
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
  List<PropertyReflection<PropertyNameCollision, dynamic>> get properties => [
    $typePart,
typePart,
type$Part,
  ];

  @override
  final AdditionalPropertiesPart<PropertyNameCollision, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(PropertyNameCollision instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(PropertyNameCollision instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<PropertyNameCollision, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  PropertyNameCollision empty() {
    return PropertyNameCollision(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is PropertyNameCollisionReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


