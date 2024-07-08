// Model reflection

part of 'basque_pig.dart';


//class reflection

class BasquePigReflection extends ClassReflection<BasquePig> {
  static BasquePigReflection instanceGetter() => instance;
  static const instance = BasquePigReflection._(
    modelName: r'BasquePig',
    className: r'BasquePig',
    classNamePart: PropertyReflection<BasquePig, 
            String

>(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _classNameGetter,
      setter: _classNameSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<BasquePig, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const BasquePigReflection._({
    required this.modelName,
    required this.className,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<BasquePig, 
            String

> classNamePart;
  static 
            String

 _classNameGetter(BasquePig parent) {
    return parent.className;
  }
  static void _classNameSetter(BasquePig parent, 
            String

 value) {
    parent.className = value;
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
  List<PropertyReflection<BasquePig, dynamic>> get properties => [
    classNamePart,
  ];

  final AdditionalPropertiesReflection<BasquePig, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<BasquePig, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<BasquePig, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => BasquePig.canDeserialize(src);
  @override
  BasquePig Function(Object? src) get deserializeFunction =>
      (src) => BasquePig.deserialize(src);

  @override
  Object? Function(BasquePig src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of BasquePig.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  BasquePig example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = BasquePig(
      className: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[classNamePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return result;
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class BasquePigXmlReflection {
    const BasquePigXmlReflection();
}

