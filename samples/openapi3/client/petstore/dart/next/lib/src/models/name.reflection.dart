// Model reflection

part of 'name.dart';


//class reflection

class NameReflection extends ClassReflection<Name> {
  static NameReflection instanceGetter() => instance;
  static const instance = NameReflection._(
    modelName: r'Name',
    className: r'Name',
    namePart: PropertyReflection<Name, 
            int

>(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _nameGetter,
      setter: _nameSetter,
    ),
    snakeCasePart: PropertyReflection<Name, UndefinedWrapper<
            int

>>(
      dartName: r'snakeCase',
      nullable: false,
      required: false,
      oasName: r'snake_case',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _snakeCaseGetter,
      setter: _snakeCaseSetter,
    ),
    propertyPart: PropertyReflection<Name, UndefinedWrapper<
            String

>>(
      dartName: r'property',
      nullable: false,
      required: false,
      oasName: r'property',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _propertyGetter,
      setter: _propertySetter,
    ),
    $123numberPart: PropertyReflection<Name, UndefinedWrapper<
            int

>>(
      dartName: r'$123number',
      nullable: false,
      required: false,
      oasName: r'123Number',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _$123numberGetter,
      setter: _$123numberSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Name, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const NameReflection._({
    required this.modelName,
    required this.className,
    required this.namePart,
    required this.snakeCasePart,
    required this.propertyPart,
    required this.$123numberPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Name, 
            int

> namePart;
  static 
            int

 _nameGetter(Name parent) {
    return parent.name;
  }
  static void _nameSetter(Name parent, 
            int

 value) {
    parent.name = value;
  }
  final PropertyReflection<Name, UndefinedWrapper<
            int

>> snakeCasePart;
  static UndefinedWrapper<
            int

> _snakeCaseGetter(Name parent) {
    return parent.snakeCase;
  }
  static void _snakeCaseSetter(Name parent, UndefinedWrapper<
            int

> value) {
    parent.snakeCase = value;
  }
  final PropertyReflection<Name, UndefinedWrapper<
            String

>> propertyPart;
  static UndefinedWrapper<
            String

> _propertyGetter(Name parent) {
    return parent.property;
  }
  static void _propertySetter(Name parent, UndefinedWrapper<
            String

> value) {
    parent.property = value;
  }
  final PropertyReflection<Name, UndefinedWrapper<
            int

>> $123numberPart;
  static UndefinedWrapper<
            int

> _$123numberGetter(Name parent) {
    return parent.$123number;
  }
  static void _$123numberSetter(Name parent, UndefinedWrapper<
            int

> value) {
    parent.$123number = value;
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
  List<PropertyReflection<Name, dynamic>> get properties => [
    namePart,
snakeCasePart,
propertyPart,
$123numberPart,
  ];

  final AdditionalPropertiesReflection<Name, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Name, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Name, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Name.canDeserialize(src);
  @override
  Name Function(Object? src) get deserializeFunction =>
      (src) => Name.deserialize(src);

  @override
  Object? Function(Name src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Name.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Name example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = Name(
      name: () {
        var result = 


            
            


    
    exampleint()


;
        return result;
      } (),
      snakeCase: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      property: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[propertyPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      $123number: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class NameXmlReflection {
    const NameXmlReflection();
}

