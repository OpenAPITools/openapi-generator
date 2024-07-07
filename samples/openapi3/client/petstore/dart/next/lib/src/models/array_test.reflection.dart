// Model reflection

part of 'array_test.dart';


//class reflection

class ArrayTestReflection extends ClassReflection<ArrayTest> {
  static ArrayTestReflection instanceGetter() => instance;
  static const instance = ArrayTestReflection._(
    modelName: r'ArrayTest',
    className: r'ArrayTest',
    arrayOfStringPart: PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
            String
>
>>(
      dartName: r'arrayOfString',
      nullable: false,
      required: false,
      oasName: r'array_of_string',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<ArrayTest, 
            String
>(parentReflectionGetter: instanceGetter,),
      getter: _arrayOfStringGetter,
      setter: _arrayOfStringSetter,
    ),
    arrayArrayOfIntegerPart: PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
>>(
      dartName: r'arrayArrayOfInteger',
      nullable: false,
      required: false,
      oasName: r'array_array_of_integer',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<ArrayTest, 
    List<
        
            int
>
>(parentReflectionGetter: instanceGetter,itemsReflection: ItemsReflection<ArrayTest, 
            int
>(parentReflectionGetter: instanceGetter,)),
      getter: _arrayArrayOfIntegerGetter,
      setter: _arrayArrayOfIntegerSetter,
    ),
    arrayArrayOfModelPart: PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
>>(
      dartName: r'arrayArrayOfModel',
      nullable: false,
      required: false,
      oasName: r'array_array_of_model',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<ArrayTest, 
    List<
        
            ReadOnlyFirst
>
>(parentReflectionGetter: instanceGetter,itemsReflection: ItemsReflection<ArrayTest, 
            ReadOnlyFirst
>(parentReflectionGetter: instanceGetter,classReflection: ReadOnlyFirstReflection.instance,)),
      getter: _arrayArrayOfModelGetter,
      setter: _arrayArrayOfModelSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayTest, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ArrayTestReflection._({
    required this.modelName,
    required this.className,
    required this.arrayOfStringPart,
    required this.arrayArrayOfIntegerPart,
    required this.arrayArrayOfModelPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
            String
>
>> arrayOfStringPart;
  static UndefinedWrapper<
    List<
        
            String
>
> _arrayOfStringGetter(ArrayTest parent) {
    return parent.arrayOfString;
  }
  static void _arrayOfStringSetter(ArrayTest parent, UndefinedWrapper<
    List<
        
            String
>
> value) {
    parent.arrayOfString = value;
  }
  final PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
>> arrayArrayOfIntegerPart;
  static UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
> _arrayArrayOfIntegerGetter(ArrayTest parent) {
    return parent.arrayArrayOfInteger;
  }
  static void _arrayArrayOfIntegerSetter(ArrayTest parent, UndefinedWrapper<
    List<
        
    List<
        
            int
>
>
> value) {
    parent.arrayArrayOfInteger = value;
  }
  final PropertyReflection<ArrayTest, UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
>> arrayArrayOfModelPart;
  static UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
> _arrayArrayOfModelGetter(ArrayTest parent) {
    return parent.arrayArrayOfModel;
  }
  static void _arrayArrayOfModelSetter(ArrayTest parent, UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst
>
>
> value) {
    parent.arrayArrayOfModel = value;
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
  List<PropertyReflection<ArrayTest, dynamic>> get properties => [
    arrayOfStringPart,
arrayArrayOfIntegerPart,
arrayArrayOfModelPart,
  ];

  final AdditionalPropertiesReflection<ArrayTest, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ArrayTest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ArrayTest, dynamic>> get allOfs => [
    
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

  /// Gets an example of ArrayTest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayTest example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ArrayTest(
      arrayOfString: () {
        PartReflection? _partReflection = _reflection.arrayOfStringPart;
        
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleString()


; })



);
      }(),
      arrayArrayOfInteger: () {
        PartReflection? _partReflection = _reflection.arrayArrayOfIntegerPart;
        
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    exampleint()


; })



; })



);
      }(),
      arrayArrayOfModel: () {
        PartReflection? _partReflection = _reflection.arrayArrayOfModelPart;
        
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    ReadOnlyFirst.$reflection.example()
    


; })



; })



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class ArrayTestXmlReflection {
    const ArrayTestXmlReflection();
}

