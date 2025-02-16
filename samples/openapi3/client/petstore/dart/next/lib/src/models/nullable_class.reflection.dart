// Model reflection

part of 'nullable_class.dart';


//class reflection

class NullableClassReflection extends ModelReflection<NullableClass> {
  static NullableClassReflection instanceGetter() => instance;
  static const instance = NullableClassReflection._(
    modelName: r'NullableClass',
    className: r'NullableClass',
    xml: XmlReflection(
),
    integerPropPart: PropertyReflection<NullableClass, UndefinedWrapper<
            int
?>>(
      dartName: r'integerProp',
      nullable: true,
      required: false,
      oasName: r'integer_prop',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_integerPropGetter),
      setter: FunctionWrapper2(_integerPropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.forint
        
),
)
),
    ),
    numberPropPart: PropertyReflection<NullableClass, UndefinedWrapper<
            num
?>>(
      dartName: r'numberProp',
      nullable: true,
      required: false,
      oasName: r'number_prop',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_numberPropGetter),
      setter: FunctionWrapper2(_numberPropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.fornum
        
),
)
),
    ),
    booleanPropPart: PropertyReflection<NullableClass, UndefinedWrapper<
            bool
?>>(
      dartName: r'booleanProp',
      nullable: true,
      required: false,
      oasName: r'boolean_prop',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_booleanPropGetter),
      setter: FunctionWrapper2(_booleanPropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.forbool
        
),
)
),
    ),
    stringPropPart: PropertyReflection<NullableClass, UndefinedWrapper<
            String
?>>(
      dartName: r'stringProp',
      nullable: true,
      required: false,
      oasName: r'string_prop',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_stringPropGetter),
      setter: FunctionWrapper2(_stringPropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.forString
        
),
)
),
    ),
    datePropPart: PropertyReflection<NullableClass, UndefinedWrapper<
            DateTime
?>>(
      dartName: r'dateProp',
      nullable: true,
      required: false,
      oasName: r'date_prop',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_datePropGetter),
      setter: FunctionWrapper2(_datePropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.forDateTime
        
),
)
),
    ),
    datetimePropPart: PropertyReflection<NullableClass, UndefinedWrapper<
            DateTime
?>>(
      dartName: r'datetimeProp',
      nullable: true,
      required: false,
      oasName: r'datetime_prop',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_datetimePropGetter),
      setter: FunctionWrapper2(_datetimePropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.forDateTime
        
),
)
),
    ),
    arrayNullablePropPart: PropertyReflection<NullableClass, UndefinedWrapper<
    List<
        
            $FreeFormObject
>
?>>(
      dartName: r'arrayNullableProp',
      nullable: true,
      required: false,
      oasName: r'array_nullable_prop',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_arrayNullablePropGetter),
      setter: FunctionWrapper2(_arrayNullablePropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
,
)
)
),
)
),
    ),
    arrayAndItemsNullablePropPart: PropertyReflection<NullableClass, UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
?>>(
      dartName: r'arrayAndItemsNullableProp',
      nullable: true,
      required: false,
      oasName: r'array_and_items_nullable_prop',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_arrayAndItemsNullablePropGetter),
      setter: FunctionWrapper2(_arrayAndItemsNullablePropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
),
)
)
),
)
),
    ),
    arrayItemsNullablePart: PropertyReflection<NullableClass, UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
>>(
      dartName: r'arrayItemsNullable',
      nullable: false,
      required: false,
      oasName: r'array_items_nullable',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_arrayItemsNullableGetter),
      setter: FunctionWrapper2(_arrayItemsNullableSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
),
)
)
,
)
),
    ),
    objectNullablePropPart: PropertyReflection<NullableClass, UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
>
?>>(
      dartName: r'objectNullableProp',
      nullable: true,
      required: false,
      oasName: r'object_nullable_prop',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_objectNullablePropGetter),
      setter: FunctionWrapper2(_objectNullablePropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
,
)
)
),
)
),
    ),
    objectAndItemsNullablePropPart: PropertyReflection<NullableClass, UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
?>>(
      dartName: r'objectAndItemsNullableProp',
      nullable: true,
      required: false,
      oasName: r'object_and_items_nullable_prop',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_objectAndItemsNullablePropGetter),
      setter: FunctionWrapper2(_objectAndItemsNullablePropSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
),
)
)
),
)
),
    ),
    objectItemsNullablePart: PropertyReflection<NullableClass, UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
>>(
      dartName: r'objectItemsNullable',
      nullable: false,
      required: false,
      oasName: r'object_items_nullable',
      oasType: r'object',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_objectItemsNullableGetter),
      setter: FunctionWrapper2(_objectItemsNullableSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    MapReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
),
)
)
,
)
),
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesPart(
      parentReflectionGetter: instanceGetter,
      itemReflection: XmlReflectionWrapper(
    xml: XmlReflection(
),
    NullableReflection(
            
        
        
            
                PrimitiveReflection.for$FreeFormObject
        
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const NullableClassReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.integerPropPart,
    required this.numberPropPart,
    required this.booleanPropPart,
    required this.stringPropPart,
    required this.datePropPart,
    required this.datetimePropPart,
    required this.arrayNullablePropPart,
    required this.arrayAndItemsNullablePropPart,
    required this.arrayItemsNullablePart,
    required this.objectNullablePropPart,
    required this.objectAndItemsNullablePropPart,
    required this.objectItemsNullablePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<NullableClass, UndefinedWrapper<
            int
?>> integerPropPart;
  static UndefinedWrapper<
            int
?> _integerPropGetter(NullableClass parent) {
    return parent.integerProp;
  }
  static void _integerPropSetter(NullableClass parent, UndefinedWrapper<
            int
?> value) {
    parent.integerProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
            num
?>> numberPropPart;
  static UndefinedWrapper<
            num
?> _numberPropGetter(NullableClass parent) {
    return parent.numberProp;
  }
  static void _numberPropSetter(NullableClass parent, UndefinedWrapper<
            num
?> value) {
    parent.numberProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
            bool
?>> booleanPropPart;
  static UndefinedWrapper<
            bool
?> _booleanPropGetter(NullableClass parent) {
    return parent.booleanProp;
  }
  static void _booleanPropSetter(NullableClass parent, UndefinedWrapper<
            bool
?> value) {
    parent.booleanProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
            String
?>> stringPropPart;
  static UndefinedWrapper<
            String
?> _stringPropGetter(NullableClass parent) {
    return parent.stringProp;
  }
  static void _stringPropSetter(NullableClass parent, UndefinedWrapper<
            String
?> value) {
    parent.stringProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
            DateTime
?>> datePropPart;
  static UndefinedWrapper<
            DateTime
?> _datePropGetter(NullableClass parent) {
    return parent.dateProp;
  }
  static void _datePropSetter(NullableClass parent, UndefinedWrapper<
            DateTime
?> value) {
    parent.dateProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
            DateTime
?>> datetimePropPart;
  static UndefinedWrapper<
            DateTime
?> _datetimePropGetter(NullableClass parent) {
    return parent.datetimeProp;
  }
  static void _datetimePropSetter(NullableClass parent, UndefinedWrapper<
            DateTime
?> value) {
    parent.datetimeProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
    List<
        
            $FreeFormObject
>
?>> arrayNullablePropPart;
  static UndefinedWrapper<
    List<
        
            $FreeFormObject
>
?> _arrayNullablePropGetter(NullableClass parent) {
    return parent.arrayNullableProp;
  }
  static void _arrayNullablePropSetter(NullableClass parent, UndefinedWrapper<
    List<
        
            $FreeFormObject
>
?> value) {
    parent.arrayNullableProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
?>> arrayAndItemsNullablePropPart;
  static UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
?> _arrayAndItemsNullablePropGetter(NullableClass parent) {
    return parent.arrayAndItemsNullableProp;
  }
  static void _arrayAndItemsNullablePropSetter(NullableClass parent, UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
?> value) {
    parent.arrayAndItemsNullableProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
>> arrayItemsNullablePart;
  static UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
> _arrayItemsNullableGetter(NullableClass parent) {
    return parent.arrayItemsNullable;
  }
  static void _arrayItemsNullableSetter(NullableClass parent, UndefinedWrapper<
    List<
        
            $FreeFormObject
?>
> value) {
    parent.arrayItemsNullable = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
>
?>> objectNullablePropPart;
  static UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
>
?> _objectNullablePropGetter(NullableClass parent) {
    return parent.objectNullableProp;
  }
  static void _objectNullablePropSetter(NullableClass parent, UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
>
?> value) {
    parent.objectNullableProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
?>> objectAndItemsNullablePropPart;
  static UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
?> _objectAndItemsNullablePropGetter(NullableClass parent) {
    return parent.objectAndItemsNullableProp;
  }
  static void _objectAndItemsNullablePropSetter(NullableClass parent, UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
?> value) {
    parent.objectAndItemsNullableProp = value;
  }

  final PropertyReflection<NullableClass, UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
>> objectItemsNullablePart;
  static UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
> _objectItemsNullableGetter(NullableClass parent) {
    return parent.objectItemsNullable;
  }
  static void _objectItemsNullableSetter(NullableClass parent, UndefinedWrapper<
    Map<String, 
        
            $FreeFormObject
?>
> value) {
    parent.objectItemsNullable = value;
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
  List<PropertyReflection<NullableClass, dynamic>> get properties => [
    integerPropPart,
numberPropPart,
booleanPropPart,
stringPropPart,
datePropPart,
datetimePropPart,
arrayNullablePropPart,
arrayAndItemsNullablePropPart,
arrayItemsNullablePart,
objectNullablePropPart,
objectAndItemsNullablePropPart,
objectItemsNullablePart,
  ];

  @override
  final AdditionalPropertiesPart<NullableClass, 
            $FreeFormObject
?>? additionalPropertiesPart;

  static AdditionalProperties<
            $FreeFormObject
?> _AdditionalPropertiesGetter(NullableClass instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(NullableClass instance, AdditionalProperties<
            $FreeFormObject
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<NullableClass, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  NullableClass empty() {
    return NullableClass(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is NullableClassReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


