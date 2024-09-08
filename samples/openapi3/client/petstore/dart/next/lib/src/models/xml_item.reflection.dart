// Model reflection

part of 'xml_item.dart';


//class reflection

class XmlItemReflection extends ModelReflection<XmlItem> {
  static XmlItemReflection instanceGetter() => instance;
  static const instance = XmlItemReflection._(
    modelName: r'XmlItem',
    className: r'XmlItem',
    xml: XmlReflection(
    namespace: r'http://a.com/schema',
    prefix: r'pre',
),
    attributeStringPart: PropertyReflection<XmlItem, UndefinedWrapper<
            String
>>(
      dartName: r'attributeString',
      nullable: false,
      required: false,
      oasName: r'attribute_string',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    attribute: true,
),
      getter: FunctionWrapper1(_attributeStringGetter),
      setter: FunctionWrapper2(_attributeStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    attribute: true,
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    attributeNumberPart: PropertyReflection<XmlItem, UndefinedWrapper<
            num
>>(
      dartName: r'attributeNumber',
      nullable: false,
      required: false,
      oasName: r'attribute_number',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    attribute: true,
),
      getter: FunctionWrapper1(_attributeNumberGetter),
      setter: FunctionWrapper2(_attributeNumberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    attribute: true,
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
),
    ),
    attributeIntegerPart: PropertyReflection<XmlItem, UndefinedWrapper<
            int
>>(
      dartName: r'attributeInteger',
      nullable: false,
      required: false,
      oasName: r'attribute_integer',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    attribute: true,
),
      getter: FunctionWrapper1(_attributeIntegerGetter),
      setter: FunctionWrapper2(_attributeIntegerSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    attribute: true,
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    attributeBooleanPart: PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>>(
      dartName: r'attributeBoolean',
      nullable: false,
      required: false,
      oasName: r'attribute_boolean',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    attribute: true,
),
      getter: FunctionWrapper1(_attributeBooleanGetter),
      setter: FunctionWrapper2(_attributeBooleanSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    attribute: true,
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    wrappedArrayPart: PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>>(
      dartName: r'wrappedArray',
      nullable: false,
      required: false,
      oasName: r'wrapped_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    wrapped: true,
),
      getter: FunctionWrapper1(_wrappedArrayGetter),
      setter: FunctionWrapper2(_wrappedArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    wrapped: true,
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
)
,
)
),
    ),
    nameStringPart: PropertyReflection<XmlItem, UndefinedWrapper<
            String
>>(
      dartName: r'nameString',
      nullable: false,
      required: false,
      oasName: r'name_string',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    xmlName: r'xml_name_string',
),
      getter: FunctionWrapper1(_nameStringGetter),
      setter: FunctionWrapper2(_nameStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'xml_name_string',
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    nameNumberPart: PropertyReflection<XmlItem, UndefinedWrapper<
            num
>>(
      dartName: r'nameNumber',
      nullable: false,
      required: false,
      oasName: r'name_number',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    xmlName: r'xml_name_number',
),
      getter: FunctionWrapper1(_nameNumberGetter),
      setter: FunctionWrapper2(_nameNumberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'xml_name_number',
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
),
    ),
    nameIntegerPart: PropertyReflection<XmlItem, UndefinedWrapper<
            int
>>(
      dartName: r'nameInteger',
      nullable: false,
      required: false,
      oasName: r'name_integer',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    xmlName: r'xml_name_integer',
),
      getter: FunctionWrapper1(_nameIntegerGetter),
      setter: FunctionWrapper2(_nameIntegerSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'xml_name_integer',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    nameBooleanPart: PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>>(
      dartName: r'nameBoolean',
      nullable: false,
      required: false,
      oasName: r'name_boolean',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    xmlName: r'xml_name_boolean',
),
      getter: FunctionWrapper1(_nameBooleanGetter),
      setter: FunctionWrapper2(_nameBooleanSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'xml_name_boolean',
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    nameArrayPart: PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>>(
      dartName: r'nameArray',
      nullable: false,
      required: false,
      oasName: r'name_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_nameArrayGetter),
      setter: FunctionWrapper2(_nameArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'xml_name_array_item',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
)
,
)
),
    ),
    nameWrappedArrayPart: PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>>(
      dartName: r'nameWrappedArray',
      nullable: false,
      required: false,
      oasName: r'name_wrapped_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    xmlName: r'xml_name_wrapped_array',
    wrapped: true,
),
      getter: FunctionWrapper1(_nameWrappedArrayGetter),
      setter: FunctionWrapper2(_nameWrappedArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'xml_name_wrapped_array',
    wrapped: true,
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    xmlName: r'xml_name_wrapped_array_item',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
)
,
)
),
    ),
    prefixStringPart: PropertyReflection<XmlItem, UndefinedWrapper<
            String
>>(
      dartName: r'prefixString',
      nullable: false,
      required: false,
      oasName: r'prefix_string',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    prefix: r'ab',
),
      getter: FunctionWrapper1(_prefixStringGetter),
      setter: FunctionWrapper2(_prefixStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    prefix: r'ab',
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    prefixNumberPart: PropertyReflection<XmlItem, UndefinedWrapper<
            num
>>(
      dartName: r'prefixNumber',
      nullable: false,
      required: false,
      oasName: r'prefix_number',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    prefix: r'cd',
),
      getter: FunctionWrapper1(_prefixNumberGetter),
      setter: FunctionWrapper2(_prefixNumberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    prefix: r'cd',
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
),
    ),
    prefixIntegerPart: PropertyReflection<XmlItem, UndefinedWrapper<
            int
>>(
      dartName: r'prefixInteger',
      nullable: false,
      required: false,
      oasName: r'prefix_integer',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    prefix: r'ef',
),
      getter: FunctionWrapper1(_prefixIntegerGetter),
      setter: FunctionWrapper2(_prefixIntegerSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    prefix: r'ef',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    prefixBooleanPart: PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>>(
      dartName: r'prefixBoolean',
      nullable: false,
      required: false,
      oasName: r'prefix_boolean',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    prefix: r'gh',
),
      getter: FunctionWrapper1(_prefixBooleanGetter),
      setter: FunctionWrapper2(_prefixBooleanSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    prefix: r'gh',
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    prefixArrayPart: PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>>(
      dartName: r'prefixArray',
      nullable: false,
      required: false,
      oasName: r'prefix_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_prefixArrayGetter),
      setter: FunctionWrapper2(_prefixArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    prefix: r'ij',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
)
,
)
),
    ),
    prefixWrappedArrayPart: PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>>(
      dartName: r'prefixWrappedArray',
      nullable: false,
      required: false,
      oasName: r'prefix_wrapped_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    prefix: r'kl',
    wrapped: true,
),
      getter: FunctionWrapper1(_prefixWrappedArrayGetter),
      setter: FunctionWrapper2(_prefixWrappedArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    prefix: r'kl',
    wrapped: true,
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    prefix: r'mn',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
)
,
)
),
    ),
    namespaceStringPart: PropertyReflection<XmlItem, UndefinedWrapper<
            String
>>(
      dartName: r'namespaceString',
      nullable: false,
      required: false,
      oasName: r'namespace_string',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://a.com/schema',
),
      getter: FunctionWrapper1(_namespaceStringGetter),
      setter: FunctionWrapper2(_namespaceStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://a.com/schema',
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    namespaceNumberPart: PropertyReflection<XmlItem, UndefinedWrapper<
            num
>>(
      dartName: r'namespaceNumber',
      nullable: false,
      required: false,
      oasName: r'namespace_number',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://b.com/schema',
),
      getter: FunctionWrapper1(_namespaceNumberGetter),
      setter: FunctionWrapper2(_namespaceNumberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://b.com/schema',
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
),
    ),
    namespaceIntegerPart: PropertyReflection<XmlItem, UndefinedWrapper<
            int
>>(
      dartName: r'namespaceInteger',
      nullable: false,
      required: false,
      oasName: r'namespace_integer',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://c.com/schema',
),
      getter: FunctionWrapper1(_namespaceIntegerGetter),
      setter: FunctionWrapper2(_namespaceIntegerSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://c.com/schema',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    namespaceBooleanPart: PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>>(
      dartName: r'namespaceBoolean',
      nullable: false,
      required: false,
      oasName: r'namespace_boolean',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://d.com/schema',
),
      getter: FunctionWrapper1(_namespaceBooleanGetter),
      setter: FunctionWrapper2(_namespaceBooleanSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://d.com/schema',
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    namespaceArrayPart: PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>>(
      dartName: r'namespaceArray',
      nullable: false,
      required: false,
      oasName: r'namespace_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_namespaceArrayGetter),
      setter: FunctionWrapper2(_namespaceArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://e.com/schema',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
)
,
)
),
    ),
    namespaceWrappedArrayPart: PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>>(
      dartName: r'namespaceWrappedArray',
      nullable: false,
      required: false,
      oasName: r'namespace_wrapped_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://f.com/schema',
    wrapped: true,
),
      getter: FunctionWrapper1(_namespaceWrappedArrayGetter),
      setter: FunctionWrapper2(_namespaceWrappedArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://f.com/schema',
    wrapped: true,
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://g.com/schema',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
)
,
)
),
    ),
    prefixNsStringPart: PropertyReflection<XmlItem, UndefinedWrapper<
            String
>>(
      dartName: r'prefixNsString',
      nullable: false,
      required: false,
      oasName: r'prefix_ns_string',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://a.com/schema',
    prefix: r'a',
),
      getter: FunctionWrapper1(_prefixNsStringGetter),
      setter: FunctionWrapper2(_prefixNsStringSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://a.com/schema',
    prefix: r'a',
),
    
            
        
        
            
                PrimitiveReflection.forString
        
,
)
),
    ),
    prefixNsNumberPart: PropertyReflection<XmlItem, UndefinedWrapper<
            num
>>(
      dartName: r'prefixNsNumber',
      nullable: false,
      required: false,
      oasName: r'prefix_ns_number',
      oasType: r'number',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://b.com/schema',
    prefix: r'b',
),
      getter: FunctionWrapper1(_prefixNsNumberGetter),
      setter: FunctionWrapper2(_prefixNsNumberSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://b.com/schema',
    prefix: r'b',
),
    
            
        
        
            
                PrimitiveReflection.fornum
        
,
)
),
    ),
    prefixNsIntegerPart: PropertyReflection<XmlItem, UndefinedWrapper<
            int
>>(
      dartName: r'prefixNsInteger',
      nullable: false,
      required: false,
      oasName: r'prefix_ns_integer',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://c.com/schema',
    prefix: r'c',
),
      getter: FunctionWrapper1(_prefixNsIntegerGetter),
      setter: FunctionWrapper2(_prefixNsIntegerSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://c.com/schema',
    prefix: r'c',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    prefixNsBooleanPart: PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>>(
      dartName: r'prefixNsBoolean',
      nullable: false,
      required: false,
      oasName: r'prefix_ns_boolean',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://d.com/schema',
    prefix: r'd',
),
      getter: FunctionWrapper1(_prefixNsBooleanGetter),
      setter: FunctionWrapper2(_prefixNsBooleanSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://d.com/schema',
    prefix: r'd',
),
    
            
        
        
            
                PrimitiveReflection.forbool
        
,
)
),
    ),
    prefixNsArrayPart: PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>>(
      dartName: r'prefixNsArray',
      nullable: false,
      required: false,
      oasName: r'prefix_ns_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_prefixNsArrayGetter),
      setter: FunctionWrapper2(_prefixNsArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://e.com/schema',
    prefix: r'e',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
)
,
)
),
    ),
    prefixNsWrappedArrayPart: PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>>(
      dartName: r'prefixNsWrappedArray',
      nullable: false,
      required: false,
      oasName: r'prefix_ns_wrapped_array',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
    namespace: r'http://f.com/schema',
    prefix: r'f',
    wrapped: true,
),
      getter: FunctionWrapper1(_prefixNsWrappedArrayGetter),
      setter: FunctionWrapper2(_prefixNsWrappedArraySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://f.com/schema',
    prefix: r'f',
    wrapped: true,
),
    
    ListReflection(XmlReflectionWrapper(
    xml: XmlReflection(
    namespace: r'http://g.com/schema',
    prefix: r'g',
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
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
    NullableReflection(ObjectReflection()
),
)
,
      getter: FunctionWrapper1(_AdditionalPropertiesGetter),
      setter: FunctionWrapper2(_AdditionalPropertiesSetter),
    ),
  );
  const XmlItemReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.attributeStringPart,
    required this.attributeNumberPart,
    required this.attributeIntegerPart,
    required this.attributeBooleanPart,
    required this.wrappedArrayPart,
    required this.nameStringPart,
    required this.nameNumberPart,
    required this.nameIntegerPart,
    required this.nameBooleanPart,
    required this.nameArrayPart,
    required this.nameWrappedArrayPart,
    required this.prefixStringPart,
    required this.prefixNumberPart,
    required this.prefixIntegerPart,
    required this.prefixBooleanPart,
    required this.prefixArrayPart,
    required this.prefixWrappedArrayPart,
    required this.namespaceStringPart,
    required this.namespaceNumberPart,
    required this.namespaceIntegerPart,
    required this.namespaceBooleanPart,
    required this.namespaceArrayPart,
    required this.namespaceWrappedArrayPart,
    required this.prefixNsStringPart,
    required this.prefixNsNumberPart,
    required this.prefixNsIntegerPart,
    required this.prefixNsBooleanPart,
    required this.prefixNsArrayPart,
    required this.prefixNsWrappedArrayPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<XmlItem, UndefinedWrapper<
            String
>> attributeStringPart;
  static UndefinedWrapper<
            String
> _attributeStringGetter(XmlItem parent) {
    return parent.attributeString;
  }
  static void _attributeStringSetter(XmlItem parent, UndefinedWrapper<
            String
> value) {
    parent.attributeString = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            num
>> attributeNumberPart;
  static UndefinedWrapper<
            num
> _attributeNumberGetter(XmlItem parent) {
    return parent.attributeNumber;
  }
  static void _attributeNumberSetter(XmlItem parent, UndefinedWrapper<
            num
> value) {
    parent.attributeNumber = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            int
>> attributeIntegerPart;
  static UndefinedWrapper<
            int
> _attributeIntegerGetter(XmlItem parent) {
    return parent.attributeInteger;
  }
  static void _attributeIntegerSetter(XmlItem parent, UndefinedWrapper<
            int
> value) {
    parent.attributeInteger = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>> attributeBooleanPart;
  static UndefinedWrapper<
            bool
> _attributeBooleanGetter(XmlItem parent) {
    return parent.attributeBoolean;
  }
  static void _attributeBooleanSetter(XmlItem parent, UndefinedWrapper<
            bool
> value) {
    parent.attributeBoolean = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>> wrappedArrayPart;
  static UndefinedWrapper<
    List<
        
            int
>
> _wrappedArrayGetter(XmlItem parent) {
    return parent.wrappedArray;
  }
  static void _wrappedArraySetter(XmlItem parent, UndefinedWrapper<
    List<
        
            int
>
> value) {
    parent.wrappedArray = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            String
>> nameStringPart;
  static UndefinedWrapper<
            String
> _nameStringGetter(XmlItem parent) {
    return parent.nameString;
  }
  static void _nameStringSetter(XmlItem parent, UndefinedWrapper<
            String
> value) {
    parent.nameString = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            num
>> nameNumberPart;
  static UndefinedWrapper<
            num
> _nameNumberGetter(XmlItem parent) {
    return parent.nameNumber;
  }
  static void _nameNumberSetter(XmlItem parent, UndefinedWrapper<
            num
> value) {
    parent.nameNumber = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            int
>> nameIntegerPart;
  static UndefinedWrapper<
            int
> _nameIntegerGetter(XmlItem parent) {
    return parent.nameInteger;
  }
  static void _nameIntegerSetter(XmlItem parent, UndefinedWrapper<
            int
> value) {
    parent.nameInteger = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>> nameBooleanPart;
  static UndefinedWrapper<
            bool
> _nameBooleanGetter(XmlItem parent) {
    return parent.nameBoolean;
  }
  static void _nameBooleanSetter(XmlItem parent, UndefinedWrapper<
            bool
> value) {
    parent.nameBoolean = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>> nameArrayPart;
  static UndefinedWrapper<
    List<
        
            int
>
> _nameArrayGetter(XmlItem parent) {
    return parent.nameArray;
  }
  static void _nameArraySetter(XmlItem parent, UndefinedWrapper<
    List<
        
            int
>
> value) {
    parent.nameArray = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>> nameWrappedArrayPart;
  static UndefinedWrapper<
    List<
        
            int
>
> _nameWrappedArrayGetter(XmlItem parent) {
    return parent.nameWrappedArray;
  }
  static void _nameWrappedArraySetter(XmlItem parent, UndefinedWrapper<
    List<
        
            int
>
> value) {
    parent.nameWrappedArray = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            String
>> prefixStringPart;
  static UndefinedWrapper<
            String
> _prefixStringGetter(XmlItem parent) {
    return parent.prefixString;
  }
  static void _prefixStringSetter(XmlItem parent, UndefinedWrapper<
            String
> value) {
    parent.prefixString = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            num
>> prefixNumberPart;
  static UndefinedWrapper<
            num
> _prefixNumberGetter(XmlItem parent) {
    return parent.prefixNumber;
  }
  static void _prefixNumberSetter(XmlItem parent, UndefinedWrapper<
            num
> value) {
    parent.prefixNumber = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            int
>> prefixIntegerPart;
  static UndefinedWrapper<
            int
> _prefixIntegerGetter(XmlItem parent) {
    return parent.prefixInteger;
  }
  static void _prefixIntegerSetter(XmlItem parent, UndefinedWrapper<
            int
> value) {
    parent.prefixInteger = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>> prefixBooleanPart;
  static UndefinedWrapper<
            bool
> _prefixBooleanGetter(XmlItem parent) {
    return parent.prefixBoolean;
  }
  static void _prefixBooleanSetter(XmlItem parent, UndefinedWrapper<
            bool
> value) {
    parent.prefixBoolean = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>> prefixArrayPart;
  static UndefinedWrapper<
    List<
        
            int
>
> _prefixArrayGetter(XmlItem parent) {
    return parent.prefixArray;
  }
  static void _prefixArraySetter(XmlItem parent, UndefinedWrapper<
    List<
        
            int
>
> value) {
    parent.prefixArray = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>> prefixWrappedArrayPart;
  static UndefinedWrapper<
    List<
        
            int
>
> _prefixWrappedArrayGetter(XmlItem parent) {
    return parent.prefixWrappedArray;
  }
  static void _prefixWrappedArraySetter(XmlItem parent, UndefinedWrapper<
    List<
        
            int
>
> value) {
    parent.prefixWrappedArray = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            String
>> namespaceStringPart;
  static UndefinedWrapper<
            String
> _namespaceStringGetter(XmlItem parent) {
    return parent.namespaceString;
  }
  static void _namespaceStringSetter(XmlItem parent, UndefinedWrapper<
            String
> value) {
    parent.namespaceString = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            num
>> namespaceNumberPart;
  static UndefinedWrapper<
            num
> _namespaceNumberGetter(XmlItem parent) {
    return parent.namespaceNumber;
  }
  static void _namespaceNumberSetter(XmlItem parent, UndefinedWrapper<
            num
> value) {
    parent.namespaceNumber = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            int
>> namespaceIntegerPart;
  static UndefinedWrapper<
            int
> _namespaceIntegerGetter(XmlItem parent) {
    return parent.namespaceInteger;
  }
  static void _namespaceIntegerSetter(XmlItem parent, UndefinedWrapper<
            int
> value) {
    parent.namespaceInteger = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>> namespaceBooleanPart;
  static UndefinedWrapper<
            bool
> _namespaceBooleanGetter(XmlItem parent) {
    return parent.namespaceBoolean;
  }
  static void _namespaceBooleanSetter(XmlItem parent, UndefinedWrapper<
            bool
> value) {
    parent.namespaceBoolean = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>> namespaceArrayPart;
  static UndefinedWrapper<
    List<
        
            int
>
> _namespaceArrayGetter(XmlItem parent) {
    return parent.namespaceArray;
  }
  static void _namespaceArraySetter(XmlItem parent, UndefinedWrapper<
    List<
        
            int
>
> value) {
    parent.namespaceArray = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>> namespaceWrappedArrayPart;
  static UndefinedWrapper<
    List<
        
            int
>
> _namespaceWrappedArrayGetter(XmlItem parent) {
    return parent.namespaceWrappedArray;
  }
  static void _namespaceWrappedArraySetter(XmlItem parent, UndefinedWrapper<
    List<
        
            int
>
> value) {
    parent.namespaceWrappedArray = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            String
>> prefixNsStringPart;
  static UndefinedWrapper<
            String
> _prefixNsStringGetter(XmlItem parent) {
    return parent.prefixNsString;
  }
  static void _prefixNsStringSetter(XmlItem parent, UndefinedWrapper<
            String
> value) {
    parent.prefixNsString = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            num
>> prefixNsNumberPart;
  static UndefinedWrapper<
            num
> _prefixNsNumberGetter(XmlItem parent) {
    return parent.prefixNsNumber;
  }
  static void _prefixNsNumberSetter(XmlItem parent, UndefinedWrapper<
            num
> value) {
    parent.prefixNsNumber = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            int
>> prefixNsIntegerPart;
  static UndefinedWrapper<
            int
> _prefixNsIntegerGetter(XmlItem parent) {
    return parent.prefixNsInteger;
  }
  static void _prefixNsIntegerSetter(XmlItem parent, UndefinedWrapper<
            int
> value) {
    parent.prefixNsInteger = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
            bool
>> prefixNsBooleanPart;
  static UndefinedWrapper<
            bool
> _prefixNsBooleanGetter(XmlItem parent) {
    return parent.prefixNsBoolean;
  }
  static void _prefixNsBooleanSetter(XmlItem parent, UndefinedWrapper<
            bool
> value) {
    parent.prefixNsBoolean = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>> prefixNsArrayPart;
  static UndefinedWrapper<
    List<
        
            int
>
> _prefixNsArrayGetter(XmlItem parent) {
    return parent.prefixNsArray;
  }
  static void _prefixNsArraySetter(XmlItem parent, UndefinedWrapper<
    List<
        
            int
>
> value) {
    parent.prefixNsArray = value;
  }

  final PropertyReflection<XmlItem, UndefinedWrapper<
    List<
        
            int
>
>> prefixNsWrappedArrayPart;
  static UndefinedWrapper<
    List<
        
            int
>
> _prefixNsWrappedArrayGetter(XmlItem parent) {
    return parent.prefixNsWrappedArray;
  }
  static void _prefixNsWrappedArraySetter(XmlItem parent, UndefinedWrapper<
    List<
        
            int
>
> value) {
    parent.prefixNsWrappedArray = value;
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
  List<PropertyReflection<XmlItem, dynamic>> get properties => [
    attributeStringPart,
attributeNumberPart,
attributeIntegerPart,
attributeBooleanPart,
wrappedArrayPart,
nameStringPart,
nameNumberPart,
nameIntegerPart,
nameBooleanPart,
nameArrayPart,
nameWrappedArrayPart,
prefixStringPart,
prefixNumberPart,
prefixIntegerPart,
prefixBooleanPart,
prefixArrayPart,
prefixWrappedArrayPart,
namespaceStringPart,
namespaceNumberPart,
namespaceIntegerPart,
namespaceBooleanPart,
namespaceArrayPart,
namespaceWrappedArrayPart,
prefixNsStringPart,
prefixNsNumberPart,
prefixNsIntegerPart,
prefixNsBooleanPart,
prefixNsArrayPart,
prefixNsWrappedArrayPart,
  ];

  @override
  final AdditionalPropertiesPart<XmlItem, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(XmlItem instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(XmlItem instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<XmlItem, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  XmlItem empty() {
    return XmlItem(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is XmlItemReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


