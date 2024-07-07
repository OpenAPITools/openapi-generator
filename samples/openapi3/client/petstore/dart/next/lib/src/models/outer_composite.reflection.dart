// Model reflection

part of 'outer_composite.dart';


//class reflection

class OuterCompositeReflection extends ClassReflection<OuterComposite> {
  static OuterCompositeReflection instanceGetter() => instance;
  static const instance = OuterCompositeReflection._(
    modelName: r'OuterComposite',
    className: r'OuterComposite',
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
      getter: _myNumberGetter,
      setter: _myNumberSetter,
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
      getter: _myStringGetter,
      setter: _myStringSetter,
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
      getter: _myBooleanGetter,
      setter: _myBooleanSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<OuterComposite, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const OuterCompositeReflection._({
    required this.modelName,
    required this.className,
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
  List<PropertyReflection<OuterComposite, dynamic>> get properties => [
    myNumberPart,
myStringPart,
myBooleanPart,
  ];

  final AdditionalPropertiesReflection<OuterComposite, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<OuterComposite, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<OuterComposite, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => OuterComposite.canDeserialize(src);
  @override
  OuterComposite Function(Object? src) get deserializeFunction =>
      (src) => OuterComposite.deserialize(src);

  @override
  Object? Function(OuterComposite src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of OuterComposite.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  OuterComposite example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return OuterComposite(
      myNumber: () {
        PartReflection? _partReflection = _reflection.myNumberPart;
        
        return UndefinedWrapper(


            
            


    
    examplenum()


);
      }(),
      myString: () {
        PartReflection? _partReflection = _reflection.myStringPart;
        
        final disc = discriminators[r'my_string'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      myBoolean: () {
        PartReflection? _partReflection = _reflection.myBooleanPart;
        
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

class OuterCompositeXmlReflection {
    const OuterCompositeXmlReflection();
}

