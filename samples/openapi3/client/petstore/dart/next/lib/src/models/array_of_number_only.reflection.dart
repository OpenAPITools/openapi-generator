// Model reflection

part of 'array_of_number_only.dart';


//class reflection

class ArrayOfNumberOnlyReflection extends ClassReflection<ArrayOfNumberOnly> {
  static ArrayOfNumberOnlyReflection instanceGetter() => instance;
  static const instance = ArrayOfNumberOnlyReflection._(
    modelName: r'ArrayOfNumberOnly',
    className: r'ArrayOfNumberOnly',
    arrayNumberPart: PropertyReflection<ArrayOfNumberOnly, UndefinedWrapper<
    List<
        
            num
>
>>(
      dartName: r'arrayNumber',
      nullable: false,
      required: false,
      oasName: r'ArrayNumber',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<ArrayOfNumberOnly, 
            num
>(parentReflectionGetter: instanceGetter,),
      getter: _arrayNumberGetter,
      setter: _arrayNumberSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ArrayOfNumberOnly, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ArrayOfNumberOnlyReflection._({
    required this.modelName,
    required this.className,
    required this.arrayNumberPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ArrayOfNumberOnly, UndefinedWrapper<
    List<
        
            num
>
>> arrayNumberPart;
  static UndefinedWrapper<
    List<
        
            num
>
> _arrayNumberGetter(ArrayOfNumberOnly parent) {
    return parent.arrayNumber;
  }
  static void _arrayNumberSetter(ArrayOfNumberOnly parent, UndefinedWrapper<
    List<
        
            num
>
> value) {
    parent.arrayNumber = value;
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
  List<PropertyReflection<ArrayOfNumberOnly, dynamic>> get properties => [
    arrayNumberPart,
  ];

  final AdditionalPropertiesReflection<ArrayOfNumberOnly, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ArrayOfNumberOnly, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ArrayOfNumberOnly, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ArrayOfNumberOnly.canDeserialize(src);
  @override
  ArrayOfNumberOnly Function(Object? src) get deserializeFunction =>
      (src) => ArrayOfNumberOnly.deserialize(src);

  @override
  Object? Function(ArrayOfNumberOnly src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ArrayOfNumberOnly.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ArrayOfNumberOnly example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ArrayOfNumberOnly(
      arrayNumber: () {
        PartReflection? _partReflection = _reflection.arrayNumberPart;
        
        return UndefinedWrapper(


    exampleList(() { _partReflection = _partReflection?.itemsReflection; return 


            
            


    
    examplenum()


; })



);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class ArrayOfNumberOnlyXmlReflection {
    const ArrayOfNumberOnlyXmlReflection();
}

