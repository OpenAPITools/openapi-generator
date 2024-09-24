// Model reflection

part of 'outer_composite.dart';


//class reflection

class OuterCompositeReflection extends ModelReflection<OuterComposite> {
  static OuterCompositeReflection instanceGetter() => instance;
  static const instance = OuterCompositeReflection._(
    modelName: r'OuterComposite',
    className: r'OuterComposite',
    xml: XmlReflection(
),
    myNumberPart: PropertyReflection<OuterComposite, UndefinedWrapper<
            num
>>(
      dartName: r'myNumber',
      nullable: false,
      required: false,
      oasName: r'my_number',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_myNumberGetter),
      setter: FunctionWrapper2(_myNumberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
),
    ),
    myStringPart: PropertyReflection<OuterComposite, UndefinedWrapper<
            String
>>(
      dartName: r'myString',
      nullable: false,
      required: false,
      oasName: r'my_string',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_myStringGetter),
      setter: FunctionWrapper2(_myStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    myBooleanPart: PropertyReflection<OuterComposite, UndefinedWrapper<
            bool
>>(
      dartName: r'myBoolean',
      nullable: false,
      required: false,
      oasName: r'my_boolean',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_myBooleanGetter),
      setter: FunctionWrapper2(_myBooleanSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
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
  const OuterCompositeReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.myNumberPart,
    required this.myStringPart,
    required this.myBooleanPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<OuterComposite, UndefinedWrapper<
            num
>> myNumberPart;
  static UndefinedWrapper<
            num
> _myNumberGetter(OuterComposite parent) {
    return parent.myNumber;
  }
  static void _myNumberSetter(OuterComposite parent, UndefinedWrapper<
            num
> value) {
    parent.myNumber = value;
  }

  final PropertyReflection<OuterComposite, UndefinedWrapper<
            String
>> myStringPart;
  static UndefinedWrapper<
            String
> _myStringGetter(OuterComposite parent) {
    return parent.myString;
  }
  static void _myStringSetter(OuterComposite parent, UndefinedWrapper<
            String
> value) {
    parent.myString = value;
  }

  final PropertyReflection<OuterComposite, UndefinedWrapper<
            bool
>> myBooleanPart;
  static UndefinedWrapper<
            bool
> _myBooleanGetter(OuterComposite parent) {
    return parent.myBoolean;
  }
  static void _myBooleanSetter(OuterComposite parent, UndefinedWrapper<
            bool
> value) {
    parent.myBoolean = value;
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
  List<PropertyReflection<OuterComposite, dynamic>> get properties => [
    myNumberPart,
myStringPart,
myBooleanPart,
  ];

  @override
  final AdditionalPropertiesPart<OuterComposite, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(OuterComposite instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(OuterComposite instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<OuterComposite, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  OuterComposite empty() {
    return OuterComposite(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is OuterCompositeReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


