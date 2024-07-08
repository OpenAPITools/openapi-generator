// Model reflection

part of 'danish_pig.dart';


//class reflection

class DanishPigReflection extends ClassReflection<DanishPig> {
  static DanishPigReflection instanceGetter() => instance;
  static const instance = DanishPigReflection._(
    modelName: r'DanishPig',
    className: r'DanishPig',
    classNamePart: PropertyReflection<DanishPig, 
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
      itemsReflection: ItemsReflection<DanishPig, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const DanishPigReflection._({
    required this.modelName,
    required this.className,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<DanishPig, 
            String

> classNamePart;
  static 
            String

 _classNameGetter(DanishPig parent) {
    return parent.className;
  }
  static void _classNameSetter(DanishPig parent, 
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
  List<PropertyReflection<DanishPig, dynamic>> get properties => [
    classNamePart,
  ];

  final AdditionalPropertiesReflection<DanishPig, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<DanishPig, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<DanishPig, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => DanishPig.canDeserialize(src);
  @override
  DanishPig Function(Object? src) get deserializeFunction =>
      (src) => DanishPig.deserialize(src);

  @override
  Object? Function(DanishPig src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of DanishPig.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  DanishPig example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = DanishPig(
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


class DanishPigXmlReflection {
    const DanishPigXmlReflection();
}

