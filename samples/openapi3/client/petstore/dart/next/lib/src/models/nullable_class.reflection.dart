// Model reflection

part of 'nullable_class.dart';


//class reflection

class NullableClassReflection extends ClassReflection<NullableClass> {
  static const instance = NullableClassReflection._(
    integerProp: PropertyReflection(
      dartName: r'integerProp',
      nullable: true,
      required: false,
      oasName: r'integer_prop',
      oasType: r'integer',
      pattern: null,
    ),
    numberProp: PropertyReflection(
      dartName: r'numberProp',
      nullable: true,
      required: false,
      oasName: r'number_prop',
      oasType: r'number',
      pattern: null,
    ),
    booleanProp: PropertyReflection(
      dartName: r'booleanProp',
      nullable: true,
      required: false,
      oasName: r'boolean_prop',
      oasType: r'boolean',
      pattern: null,
    ),
    stringProp: PropertyReflection(
      dartName: r'stringProp',
      nullable: true,
      required: false,
      oasName: r'string_prop',
      oasType: r'string',
      pattern: null,
    ),
    dateProp: PropertyReflection(
      dartName: r'dateProp',
      nullable: true,
      required: false,
      oasName: r'date_prop',
      oasType: r'string',
      pattern: null,
    ),
    datetimeProp: PropertyReflection(
      dartName: r'datetimeProp',
      nullable: true,
      required: false,
      oasName: r'datetime_prop',
      oasType: r'string',
      pattern: null,
    ),
    arrayNullableProp: PropertyReflection(
      dartName: r'arrayNullableProp',
      nullable: true,
      required: false,
      oasName: r'array_nullable_prop',
      oasType: r'array',
      pattern: null,
    ),
    arrayAndItemsNullableProp: PropertyReflection(
      dartName: r'arrayAndItemsNullableProp',
      nullable: true,
      required: false,
      oasName: r'array_and_items_nullable_prop',
      oasType: r'array',
      pattern: null,
    ),
    arrayItemsNullable: PropertyReflection(
      dartName: r'arrayItemsNullable',
      nullable: false,
      required: false,
      oasName: r'array_items_nullable',
      oasType: r'array',
      pattern: null,
    ),
    objectNullableProp: PropertyReflection(
      dartName: r'objectNullableProp',
      nullable: true,
      required: false,
      oasName: r'object_nullable_prop',
      oasType: r'object',
      pattern: null,
    ),
    objectAndItemsNullableProp: PropertyReflection(
      dartName: r'objectAndItemsNullableProp',
      nullable: true,
      required: false,
      oasName: r'object_and_items_nullable_prop',
      oasType: r'object',
      pattern: null,
    ),
    objectItemsNullable: PropertyReflection(
      dartName: r'objectItemsNullable',
      nullable: false,
      required: false,
      oasName: r'object_items_nullable',
      oasType: r'object',
      pattern: null,
    ),
  );
  const NullableClassReflection._({
    required this.integerProp,
  
    required this.numberProp,
  
    required this.booleanProp,
  
    required this.stringProp,
  
    required this.dateProp,
  
    required this.datetimeProp,
  
    required this.arrayNullableProp,
  
    required this.arrayAndItemsNullableProp,
  
    required this.arrayItemsNullable,
  
    required this.objectNullableProp,
  
    required this.objectAndItemsNullableProp,
  
    required this.objectItemsNullable,
  });

  final PropertyReflection<UndefinedWrapper<
            int
?>> integerProp;
  final PropertyReflection<UndefinedWrapper<
            num
?>> numberProp;
  final PropertyReflection<UndefinedWrapper<
            bool
?>> booleanProp;
  final PropertyReflection<UndefinedWrapper<
            String
?>> stringProp;
  final PropertyReflection<UndefinedWrapper<
            DateTime
?>> dateProp;
  final PropertyReflection<UndefinedWrapper<
            DateTime
?>> datetimeProp;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            Map<String, Object?>
>
?>> arrayNullableProp;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            Map<String, Object?>
?>
?>> arrayAndItemsNullableProp;
  final PropertyReflection<UndefinedWrapper<
    List<
        
            Map<String, Object?>
?>
>> arrayItemsNullable;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
            Map<String, Object?>
>
?>> objectNullableProp;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
            Map<String, Object?>
?>
?>> objectAndItemsNullableProp;
  final PropertyReflection<UndefinedWrapper<
    Map<String, 
        
            Map<String, Object?>
?>
>> objectItemsNullable;

  @override
  List<PropertyReflection> get members => [
    integerProp,
numberProp,
booleanProp,
stringProp,
dateProp,
datetimeProp,
arrayNullableProp,
arrayAndItemsNullableProp,
arrayItemsNullable,
objectNullableProp,
objectAndItemsNullableProp,
objectItemsNullable,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NullableClass.canDeserialize(src);
  @override
  NullableClass Function(Object? src) get deserializeFunction =>
      (src) => NullableClass.deserialize(src);

  @override
  Object? Function(NullableClass src) get serializeFunction =>
      (src) => src.serialize();
}

class NullableClassXmlReflection {
    const NullableClassXmlReflection();
}

