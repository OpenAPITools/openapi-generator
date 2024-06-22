// Model reflection

part of 'order.dart';


//class reflection

class OrderReflection extends ClassReflection<Order> {
  static const instance = OrderReflection._(
    id: PropertyReflection(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
    ),
    petId: PropertyReflection(
      dartName: r'petId',
      nullable: false,
      required: false,
      oasName: r'petId',
      oasType: r'integer',
      pattern: null,
    ),
    quantity: PropertyReflection(
      dartName: r'quantity',
      nullable: false,
      required: false,
      oasName: r'quantity',
      oasType: r'integer',
      pattern: null,
    ),
    shipDate: PropertyReflection(
      dartName: r'shipDate',
      nullable: false,
      required: false,
      oasName: r'shipDate',
      oasType: r'string',
      pattern: null,
    ),
    status: PropertyReflection(
      dartName: r'status',
      nullable: false,
      required: false,
      oasName: r'status',
      oasType: r'string',
      pattern: null,
    ),
    complete: PropertyReflection(
      dartName: r'complete',
      nullable: false,
      required: false,
      oasName: r'complete',
      oasType: r'boolean',
      pattern: null,
    ),
  );
  const OrderReflection._({
    required this.id,
  
    required this.petId,
  
    required this.quantity,
  
    required this.shipDate,
  
    required this.status,
  
    required this.complete,
  });

  final PropertyReflection<UndefinedWrapper<
            int
>> id;
  final PropertyReflection<UndefinedWrapper<
            int
>> petId;
  final PropertyReflection<UndefinedWrapper<
            int
>> quantity;
  final PropertyReflection<UndefinedWrapper<
            DateTime
>> shipDate;
  final PropertyReflection<UndefinedWrapper<
            OrderStatusEnum
>> status;
  final PropertyReflection<UndefinedWrapper<
            bool
>> complete;

  @override
  List<PropertyReflection> get members => [
    id,
petId,
quantity,
shipDate,
status,
complete,
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
}

class OrderXmlReflection {
    const OrderXmlReflection();
}

