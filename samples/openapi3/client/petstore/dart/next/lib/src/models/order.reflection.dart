// Model reflection

part of 'order.dart';


//class reflection

class OrderReflection extends ModelReflection<Order> {
  static OrderReflection instanceGetter() => instance;
  static const instance = OrderReflection._(
    modelName: r'Order',
    className: r'Order',
    xml: XmlReflection(
    xmlName: r'Order',
),
    idPart: PropertyReflection<Order, UndefinedWrapper<
            int
>>(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_idGetter),
      setter: FunctionWrapper2(_idSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    petIdPart: PropertyReflection<Order, UndefinedWrapper<
            int
>>(
      dartName: r'petId',
      nullable: false,
      required: false,
      oasName: r'petId',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_petIdGetter),
      setter: FunctionWrapper2(_petIdSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    quantityPart: PropertyReflection<Order, UndefinedWrapper<
            int
>>(
      dartName: r'quantity',
      nullable: false,
      required: false,
      oasName: r'quantity',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_quantityGetter),
      setter: FunctionWrapper2(_quantitySetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forint
        
,
)
),
    ),
    shipDatePart: PropertyReflection<Order, UndefinedWrapper<
            DateTime
>>(
      dartName: r'shipDate',
      nullable: false,
      required: false,
      oasName: r'shipDate',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_shipDateGetter),
      setter: FunctionWrapper2(_shipDateSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            
        
        
            
                PrimitiveReflection.forDateTime
        
,
)
),
    ),
    statusPart: PropertyReflection<Order, UndefinedWrapper<
            OrderStatusEnum
>>(
      dartName: r'status',
      nullable: false,
      required: false,
      oasName: r'status',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_statusGetter),
      setter: FunctionWrapper2(_statusSetter),
      reflection: UndefinedWrapperReflection(XmlReflectionWrapper(
    xml: XmlReflection(
),
    
            OrderStatusEnum.$reflection
        
        
,
)
),
    ),
    completePart: PropertyReflection<Order, UndefinedWrapper<
            bool
>>(
      dartName: r'complete',
      nullable: false,
      required: false,
      oasName: r'complete',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      xml: XmlReflection(
),
      getter: FunctionWrapper1(_completeGetter),
      setter: FunctionWrapper2(_completeSetter),
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
  const OrderReflection._({
    required this.modelName,
    required this.className,
    required this.xml,
    required this.idPart,
    required this.petIdPart,
    required this.quantityPart,
    required this.shipDatePart,
    required this.statusPart,
    required this.completePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Order, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(Order parent) {
    return parent.id;
  }
  static void _idSetter(Order parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }

  final PropertyReflection<Order, UndefinedWrapper<
            int
>> petIdPart;
  static UndefinedWrapper<
            int
> _petIdGetter(Order parent) {
    return parent.petId;
  }
  static void _petIdSetter(Order parent, UndefinedWrapper<
            int
> value) {
    parent.petId = value;
  }

  final PropertyReflection<Order, UndefinedWrapper<
            int
>> quantityPart;
  static UndefinedWrapper<
            int
> _quantityGetter(Order parent) {
    return parent.quantity;
  }
  static void _quantitySetter(Order parent, UndefinedWrapper<
            int
> value) {
    parent.quantity = value;
  }

  final PropertyReflection<Order, UndefinedWrapper<
            DateTime
>> shipDatePart;
  static UndefinedWrapper<
            DateTime
> _shipDateGetter(Order parent) {
    return parent.shipDate;
  }
  static void _shipDateSetter(Order parent, UndefinedWrapper<
            DateTime
> value) {
    parent.shipDate = value;
  }

  final PropertyReflection<Order, UndefinedWrapper<
            OrderStatusEnum
>> statusPart;
  static UndefinedWrapper<
            OrderStatusEnum
> _statusGetter(Order parent) {
    return parent.status;
  }
  static void _statusSetter(Order parent, UndefinedWrapper<
            OrderStatusEnum
> value) {
    parent.status = value;
  }

  final PropertyReflection<Order, UndefinedWrapper<
            bool
>> completePart;
  static UndefinedWrapper<
            bool
> _completeGetter(Order parent) {
    return parent.complete;
  }
  static void _completeSetter(Order parent, UndefinedWrapper<
            bool
> value) {
    parent.complete = value;
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
  List<PropertyReflection<Order, dynamic>> get properties => [
    idPart,
petIdPart,
quantityPart,
shipDatePart,
statusPart,
completePart,
  ];

  @override
  final AdditionalPropertiesPart<Order, Object
?>? additionalPropertiesPart;

  static AdditionalProperties<Object
?> _AdditionalPropertiesGetter(Order instance) {
    return instance.additionalProperties;
  }
  static void _AdditionalPropertiesSetter(Order instance, AdditionalProperties<Object
?> additionalProperties) {
    instance.additionalProperties = additionalProperties;
  }

  
  

  @override
  List<AllOfReflection<Order, Object>> get allOfs => [
    
  ];



  /// Creates an empty instance used as a starting point for deserialization.
  @override
  Order empty() {
    return Order(
    );
  }

  @override
  bool operator ==(Object other) {
    return other is OrderReflection && oasName == other.oasName;
  }

  @override
  int get hashCode => oasName.hashCode;
}


