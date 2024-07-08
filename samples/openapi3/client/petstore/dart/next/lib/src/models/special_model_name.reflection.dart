// Model reflection

part of 'special_model_name.dart';


//class reflection

class SpecialModelNameReflection extends ClassReflection<SpecialModelName> {
  static SpecialModelNameReflection instanceGetter() => instance;
  static const instance = SpecialModelNameReflection._(
    modelName: r'_special_model.name_',
    className: r'SpecialModelName',
    $specialPropertyNamePart: PropertyReflection<SpecialModelName, UndefinedWrapper<
            int

>>(
      dartName: r'$specialPropertyName',
      nullable: false,
      required: false,
      oasName: r'$special[property.name]',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _$specialPropertyNameGetter,
      setter: _$specialPropertyNameSetter,
    ),
    specialModelNamePart: PropertyReflection<SpecialModelName, UndefinedWrapper<
            String

>>(
      dartName: r'specialModelName',
      nullable: false,
      required: false,
      oasName: r'_special_model.name_',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _specialModelNameGetter,
      setter: _specialModelNameSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<SpecialModelName, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const SpecialModelNameReflection._({
    required this.modelName,
    required this.className,
    required this.$specialPropertyNamePart,
    required this.specialModelNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<SpecialModelName, UndefinedWrapper<
            int

>> $specialPropertyNamePart;
  static UndefinedWrapper<
            int

> _$specialPropertyNameGetter(SpecialModelName parent) {
    return parent.$specialPropertyName;
  }
  static void _$specialPropertyNameSetter(SpecialModelName parent, UndefinedWrapper<
            int

> value) {
    parent.$specialPropertyName = value;
  }
  final PropertyReflection<SpecialModelName, UndefinedWrapper<
            String

>> specialModelNamePart;
  static UndefinedWrapper<
            String

> _specialModelNameGetter(SpecialModelName parent) {
    return parent.specialModelName;
  }
  static void _specialModelNameSetter(SpecialModelName parent, UndefinedWrapper<
            String

> value) {
    parent.specialModelName = value;
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
  List<PropertyReflection<SpecialModelName, dynamic>> get properties => [
    $specialPropertyNamePart,
specialModelNamePart,
  ];

  final AdditionalPropertiesReflection<SpecialModelName, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<SpecialModelName, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<SpecialModelName, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => SpecialModelName.canDeserialize(src);
  @override
  SpecialModelName Function(Object? src) get deserializeFunction =>
      (src) => SpecialModelName.deserialize(src);

  @override
  Object? Function(SpecialModelName src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of SpecialModelName.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  SpecialModelName example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = SpecialModelName(
      $specialPropertyName: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      specialModelName: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[specialModelNamePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class SpecialModelNameXmlReflection {
    const SpecialModelNameXmlReflection();
}

