// Model reflection

part of 'dog.dart';


//class reflection

class DogReflection extends ClassReflection<Dog> {
  static DogReflection instanceGetter() => instance;
  static const instance = DogReflection._(
    modelName: r'Dog',
    className: r'Dog',
    colorPart: PropertyReflection<Dog, UndefinedWrapper<
            String

>>(
      dartName: r'color',
      nullable: false,
      required: false,
      oasName: r'color',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _colorGetter,
      setter: _colorSetter,
    ),
    breedPart: PropertyReflection<Dog, UndefinedWrapper<
            String

>>(
      dartName: r'breed',
      nullable: false,
      required: false,
      oasName: r'breed',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _breedGetter,
      setter: _breedSetter,
    ),
    classNamePart: PropertyReflection<Dog, 
            String

>(
      dartName: r'className',
      nullable: false,
      required: true,
      oasName: r'className',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: true,
      getter: _classNameGetter,
      setter: _classNameSetter,
    ),
    discriminatorKey: r'className',
    discriminatorImplicitMappings: const {
    },
    discriminatorMappings: const {
    },
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Dog, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const DogReflection._({
    required this.modelName,
    required this.className,
    required this.colorPart,
    required this.breedPart,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Dog, UndefinedWrapper<
            String

>> colorPart;
  static UndefinedWrapper<
            String

> _colorGetter(Dog parent) {
    return parent.color;
  }
  static void _colorSetter(Dog parent, UndefinedWrapper<
            String

> value) {
    parent.color = value;
  }
  final PropertyReflection<Dog, UndefinedWrapper<
            String

>> breedPart;
  static UndefinedWrapper<
            String

> _breedGetter(Dog parent) {
    return parent.breed;
  }
  static void _breedSetter(Dog parent, UndefinedWrapper<
            String

> value) {
    parent.breed = value;
  }
  final PropertyReflection<Dog, 
            String

> classNamePart;
  static 
            String

 _classNameGetter(Dog parent) {
    return parent.className;
  }
  static void _classNameSetter(Dog parent, 
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
  List<PropertyReflection<Dog, dynamic>> get properties => [
    colorPart,
breedPart,
classNamePart,
  ];

  final AdditionalPropertiesReflection<Dog, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Dog, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Dog, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Dog, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<Dog, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Dog.canDeserialize(src);
  @override
  Dog Function(Object? src) get deserializeFunction =>
      (src) => Dog.deserialize(src);

  @override
  Object? Function(Dog src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Dog.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Dog example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Dog(
      color: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[colorPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      breed: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[breedPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
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


class DogXmlReflection {
    const DogXmlReflection();
}

