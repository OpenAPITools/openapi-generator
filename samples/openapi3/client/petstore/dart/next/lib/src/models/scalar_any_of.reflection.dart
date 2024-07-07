// Model reflection

part of 'scalar_any_of.dart';


//class reflection

class ScalarAnyOfReflection extends ClassReflection<ScalarAnyOf> {
  static ScalarAnyOfReflection instanceGetter() => instance;
  static const instance = ScalarAnyOfReflection._(
    modelName: r'ScalarAnyOf',
    className: r'ScalarAnyOf',
    
    
    anyOf0Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
          ),
    
    anyOf1Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
          ),
    
    anyOf2Part: AnyOfReflection(
      parentReflectionGetter: instanceGetter,
          ),
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ScalarAnyOf, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ScalarAnyOfReflection._({
    required this.modelName,
    required this.className,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.anyOf0Part,
    
    required this.anyOf1Part,
    
    required this.anyOf2Part,
    
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
  List<PropertyReflection<ScalarAnyOf, dynamic>> get properties => [
      ];

  final AdditionalPropertiesReflection<ScalarAnyOf, Object
?> additionalPropertiesPart;

  
  
  final AnyOfReflection<ScalarAnyOf, 
            String
> anyOf0Part;
  
  final AnyOfReflection<ScalarAnyOf, 
            num
> anyOf1Part;
  
  final AnyOfReflection<ScalarAnyOf, 
            bool
> anyOf2Part;
  
  @override
  List<PartReflection<ScalarAnyOf, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ScalarAnyOf, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<ScalarAnyOf, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<ScalarAnyOf, dynamic>> get anyOfs => [
    anyOf0Part,anyOf1Part,anyOf2Part,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ScalarAnyOf.canDeserialize(src);
  @override
  ScalarAnyOf Function(Object? src) get deserializeFunction =>
      (src) => ScalarAnyOf.deserialize(src);

  @override
  Object? Function(ScalarAnyOf src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ScalarAnyOf.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ScalarAnyOf example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ScalarAnyOf(
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
      anyOf0: () {
        PartReflection? _partReflection = _reflection.anyOf0Part;
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      anyOf1: () {
        PartReflection? _partReflection = _reflection.anyOf1Part;
        return UndefinedWrapper(


            
            


    
    examplenum()


);
      }(),
      anyOf2: () {
        PartReflection? _partReflection = _reflection.anyOf2Part;
        return UndefinedWrapper(


            
            


    
    examplebool()


);
      }(),
    );
  }
}

class ScalarAnyOfXmlReflection {
    const ScalarAnyOfXmlReflection();
}

