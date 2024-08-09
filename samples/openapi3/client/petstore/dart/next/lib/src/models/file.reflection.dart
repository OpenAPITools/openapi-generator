// Model reflection

part of 'file.dart';


//class reflection

class FileReflection extends ClassReflection<File> {
  static FileReflection instanceGetter() => instance;
  static const instance = FileReflection._(
    modelName: r'File',
    className: r'File',
    sourceURIPart: PropertyReflection<File, UndefinedWrapper<
            String

>>(
      dartName: r'sourceURI',
      nullable: false,
      required: false,
      oasName: r'sourceURI',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _sourceURIGetter,
      setter: _sourceURISetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<File, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const FileReflection._({
    required this.modelName,
    required this.className,
    required this.sourceURIPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<File, UndefinedWrapper<
            String

>> sourceURIPart;
  static UndefinedWrapper<
            String

> _sourceURIGetter(File parent) {
    return parent.sourceURI;
  }
  static void _sourceURISetter(File parent, UndefinedWrapper<
            String

> value) {
    parent.sourceURI = value;
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
  List<PropertyReflection<File, dynamic>> get properties => [
    sourceURIPart,
  ];

  final AdditionalPropertiesReflection<File, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<File, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<File, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => File.canDeserialize(src);
  @override
  File Function(Object? src) get deserializeFunction =>
      (src) => File.deserialize(src);

  @override
  Object? Function(File src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of File.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  File example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = File(
      sourceURI: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[sourceURIPart.oasName]?.key.key;
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


class FileXmlReflection {
    const FileXmlReflection();
}

