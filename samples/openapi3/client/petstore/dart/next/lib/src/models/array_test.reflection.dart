// Model reflection

part of 'array_test.dart';


//class reflection

class ArrayTestReflection extends ClassReflection<ArrayTest> {
  static const instance = ArrayTestReflection._(
    arrayOfString: PropertyReflection(
      dartName: r'arrayOfString',
      nullable: false,
      required: false,
      oasName: r'array_of_string',
      oasType: r'array',
      pattern: null,
    ),
    arrayArrayOfInteger: PropertyReflection(
      dartName: r'arrayArrayOfInteger',
      nullable: false,
      required: false,
      oasName: r'array_array_of_integer',
      oasType: r'array',
      pattern: null,
    ),
    arrayArrayOfModel: PropertyReflection(
      dartName: r'arrayArrayOfModel',
      nullable: false,
      required: false,
      oasName: r'array_array_of_model',
      oasType: r'array',
      pattern: null,
    ),
  );
  const ArrayTestReflection._({
    required this.arrayOfString,
  
    required this.arrayArrayOfInteger,
  
    required this.arrayArrayOfModel,
  });

  final PropertyReflection<UndefinedWrapper<
    List<
        
            String
>
>> arrayOfString;
  final PropertyReflection<UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
>> arrayArrayOfInteger;
  final PropertyReflection<UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
>> arrayArrayOfModel;

  @override
  List<PropertyReflection> get members => [
    arrayOfString,
arrayArrayOfInteger,
arrayArrayOfModel,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayTest.canDeserialize(src);
  @override
  ArrayTest Function(Object? src) get deserializeFunction =>
      (src) => ArrayTest.deserialize(src);

  @override
  Object? Function(ArrayTest src) get serializeFunction =>
      (src) => src.serialize();
}

class ArrayTestXmlReflection {
    const ArrayTestXmlReflection();
}

