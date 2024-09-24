// Model reflection

part of 'zebra.dart';


//class reflection

class ZebraReflection extends ModelReflection<Zebra> {
  static ZebraReflection instanceGetter() => instance;
  static const instance = ZebraReflection._(
    modelName: r'zebra',
    className: r'Zebra',
    xml: XmlReflection(
),
    typePart: PropertyReflection<Zebra, UndefinedWrapper<
            ZebraTypeEnum
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
    
            ZebraTypeEnum.$reflection
        
        
,
)
),
    ),
    classNamePart: PropertyReflection<Zebra, 
            String
>(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_classNameGetter),
      setter: FunctionWrapper2(_classNameSetter),
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
  const ZebraReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.typePart,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Zebra, UndefinedWrapper<
            ZebraTypeEnum
>> typePart;
  static UndefinedWrapper<
            ZebraTypeEnum
> _typeGetter(Zebra parent) {
    return parent.type;
  }
  static void _typeSetter(Zebra parent, UndefinedWrapper<
            ZebraTypeEnum
> value) {
    parent.type = value;
  }

  final PropertyReflection<Zebra, 
            String
> classNamePart;
  static 
            String
 _classNameGetter(Zebra parent) {
    return parent.className;
  }
  static void _classNameSetter(Zebra parent, 
            String
 value) {
    parent.className = value;
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
  List<PropertyReflection<Zebra, dynamic>> get properties => [
    typePart,
classNamePart,
  ];

  @override
  final AdditionalPropertiesPart<Zebra, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Zebra instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Zebra instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Zebra, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Zebra empty() {
    return Zebra(
      className: classNamePart.reflection.emptyFunction(),
    );
  }

  @override
  bool operator ==(Object other) {
    return other is ZebraReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


