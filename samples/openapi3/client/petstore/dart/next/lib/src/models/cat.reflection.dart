// Model reflection

part of 'cat.dart';


//class reflection

class CatReflection extends ClassReflection<Cat> {
  static CatReflection instanceGetter() => instance;
  static const instance = CatReflection._(
    modelName: r'Cat',
    className: r'Cat',
    colorPart: PropertyReflection<Cat, UndefinedWrapper<
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
    declawedPart: PropertyReflection<Cat, UndefinedWrapper<
            bool

>>(
      dartName: r'declawed',
      nullable: false,
      required: false,
      oasName: r'declawed',
      oasType: r'boolean',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _declawedGetter,
      setter: _declawedSetter,
    ),
    classNamePart: PropertyReflection<Cat, 
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
      itemsReflection: ItemsReflection<Cat, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const CatReflection._({
    required this.modelName,
    required this.className,
    required this.colorPart,
    required this.declawedPart,
    required this.classNamePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Cat, UndefinedWrapper<
            String

>> colorPart;
  static UndefinedWrapper<
            String

> _colorGetter(Cat parent) {
    return parent.color;
  }
  static void _colorSetter(Cat parent, UndefinedWrapper<
            String

> value) {
    parent.color = value;
  }
  final PropertyReflection<Cat, UndefinedWrapper<
            bool

>> declawedPart;
  static UndefinedWrapper<
            bool

> _declawedGetter(Cat parent) {
    return parent.declawed;
  }
  static void _declawedSetter(Cat parent, UndefinedWrapper<
            bool

> value) {
    parent.declawed = value;
  }
  final PropertyReflection<Cat, 
            String

> classNamePart;
  static 
            String

 _classNameGetter(Cat parent) {
    return parent.className;
  }
  static void _classNameSetter(Cat parent, 
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
  List<PropertyReflection<Cat, dynamic>> get properties => [
    colorPart,
declawedPart,
classNamePart,
  ];

  final AdditionalPropertiesReflection<Cat, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Cat, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Cat, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<Cat, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<Cat, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Cat.canDeserialize(src);
  @override
  Cat Function(Object? src) get deserializeFunction =>
      (src) => Cat.deserialize(src);

  @override
  Object? Function(Cat src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Cat.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Cat example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Cat(
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
      declawed: () {
        var result = 


            
            


    
    examplebool()


;
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


class CatXmlReflection {
    const CatXmlReflection();
}

