// Model reflection

part of 'deprecated_object.dart';


//class reflection

class DeprecatedObjectReflection extends ClassReflection<DeprecatedObject> {
  static DeprecatedObjectReflection instanceGetter() => instance;
  static const instance = DeprecatedObjectReflection._(
    modelName: r'DeprecatedObject',
    className: r'DeprecatedObject',
    namePart: PropertyReflection<DeprecatedObject, UndefinedWrapper<
            String

>>(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _nameGetter,
      setter: _nameSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<DeprecatedObject, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const DeprecatedObjectReflection._({
    required this.modelName,
    required this.className,
    required this.namePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<DeprecatedObject, UndefinedWrapper<
            String

>> namePart;
  static UndefinedWrapper<
            String

> _nameGetter(DeprecatedObject parent) {
    return parent.name;
  }
  static void _nameSetter(DeprecatedObject parent, UndefinedWrapper<
            String

> value) {
    parent.name = value;
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
  List<PropertyReflection<DeprecatedObject, dynamic>> get properties => [
    namePart,
  ];

  final AdditionalPropertiesReflection<DeprecatedObject, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<DeprecatedObject, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<DeprecatedObject, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => DeprecatedObject.canDeserialize(src);
  @override
  DeprecatedObject Function(Object? src) get deserializeFunction =>
      (src) => DeprecatedObject.deserialize(src);

  @override
  Object? Function(DeprecatedObject src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of DeprecatedObject.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  DeprecatedObject example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = DeprecatedObject(
      name: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[namePart.oasName]?.key.key;
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


class DeprecatedObjectXmlReflection {
    const DeprecatedObjectXmlReflection();
}

