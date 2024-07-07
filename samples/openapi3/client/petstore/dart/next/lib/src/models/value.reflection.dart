// Model reflection

part of 'value.dart';


//class reflection

class ValueReflection extends ClassReflection<Value> {
  static ValueReflection instanceGetter() => instance;
  static const instance = ValueReflection._(
    modelName: r'Value',
    className: r'Value',
    
    
    oneOf0Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      classReflection: ScalarReflection.instance,
    ),
    
    oneOf1Part: OneOfReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Value, 
            Scalar
>(parentReflectionGetter: instanceGetter,classReflection: ScalarReflection.instance,),
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Value, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ValueReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.oneOf0Part,
    
    required this.oneOf1Part,
    
    required this.additionalPropertiesPart,
  });




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
  List<PropertyReflection<Value, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<Value, Object
?> additionalPropertiesPart;

  
  
  final OneOfReflection<Value, 
            Scalar
> oneOf0Part;
  
  final OneOfReflection<Value, 
    List<
        
            Scalar
>
> oneOf1Part;
  
  @override
  List<PartReflection<Value, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Value, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Value, dynamic>> get oneOfs => [
    oneOf0Part,oneOf1Part,
  ];
  @override
  List<AnyOfReflection<Value, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Value.canDeserialize(src);
  @override
  Value Function(Object? src) get deserializeFunction =>
      (src) => Value.deserialize(src);

  @override
  Object? Function(Value src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Value.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Value example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Value(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      oneOf0: () {
        PartReflection? _partReflection = _reflection.oneOf0Part;
        return UndefinedWrapper(


            
            


    Scalar.$reflection.example(discriminators: discriminators)
    


);
      }(),
      
    );
  }
}

class ValueXmlReflection {
    const ValueXmlReflection();
}

