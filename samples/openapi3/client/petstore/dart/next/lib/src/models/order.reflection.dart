// Model reflection

part of 'order.dart';


//class reflection

class OrderReflection extends ClassReflection<Order> {
  static OrderReflection instanceGetter() => instance;
  static const instance = OrderReflection._(
    modelName: r'Order',
    className: r'Order',
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
      getter: _idGetter,
      setter: _idSetter,
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
      getter: _petIdGetter,
      setter: _petIdSetter,
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
      getter: _quantityGetter,
      setter: _quantitySetter,
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
      getter: _shipDateGetter,
      setter: _shipDateSetter,
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
      getter: _statusGetter,
      setter: _statusSetter,
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
      getter: _completeGetter,
      setter: _completeSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Order, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const OrderReflection._({
    required this.modelName,
    required this.className,
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
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<Order, dynamic>> get properties => [
    idPart,
petIdPart,
quantityPart,
shipDatePart,
statusPart,
completePart,
  ];

  final AdditionalPropertiesReflection<Order, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Order, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Order, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Order.canDeserialize(src);
  @override
  Order Function(Object? src) get deserializeFunction =>
      (src) => Order.deserialize(src);

  @override
  Object? Function(Order src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Order.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Order example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Order(
      id: () {
        PartReflection? _partReflection = _reflection.idPart;
        
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
      petId: () {
        PartReflection? _partReflection = _reflection.petIdPart;
        
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
      quantity: () {
        PartReflection? _partReflection = _reflection.quantityPart;
        
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
      shipDate: () {
        PartReflection? _partReflection = _reflection.shipDatePart;
        
        return UndefinedWrapper(


            
            


    
    exampleDateTime()


);
      }(),
      status: () {
        PartReflection? _partReflection = _reflection.statusPart;
        
        return UndefinedWrapper(


            exampleEnum(OrderStatusEnum.values)



);
      }(),
      complete: () {
        PartReflection? _partReflection = _reflection.completePart;
        
        return UndefinedWrapper(


            
            


    
    examplebool()


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class OrderXmlReflection {
    const OrderXmlReflection();
}

