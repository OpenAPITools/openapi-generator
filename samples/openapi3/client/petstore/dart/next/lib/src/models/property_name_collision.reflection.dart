// Model reflection

part of 'property_name_collision.dart';


//class reflection

class PropertyNameCollisionReflection extends ClassReflection<PropertyNameCollision> {
  static PropertyNameCollisionReflection instanceGetter() => instance;
  static const instance = PropertyNameCollisionReflection._(
    modelName: r'PropertyNameCollision',
    className: r'PropertyNameCollision',
    $typePart: PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String

>>(
      dartName: r'$type',
      nullable: false,
      required: false,
      oasName: r'_type',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _$typeGetter,
      setter: _$typeSetter,
    ),
    typePart: PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String

>>(
      dartName: r'type',
      nullable: false,
      required: false,
      oasName: r'type',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _typeGetter,
      setter: _typeSetter,
    ),
    type$Part: PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String

>>(
      dartName: r'type$',
      nullable: false,
      required: false,
      oasName: r'type_',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _type$Getter,
      setter: _type$Setter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<PropertyNameCollision, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const PropertyNameCollisionReflection._({
    required this.modelName,
    required this.className,
    required this.$typePart,
    required this.typePart,
    required this.type$Part,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String

>> $typePart;
  static UndefinedWrapper<
            String

> _$typeGetter(PropertyNameCollision parent) {
    return parent.$type;
  }
  static void _$typeSetter(PropertyNameCollision parent, UndefinedWrapper<
            String

> value) {
    parent.$type = value;
  }
  final PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String

>> typePart;
  static UndefinedWrapper<
            String

> _typeGetter(PropertyNameCollision parent) {
    return parent.type;
  }
  static void _typeSetter(PropertyNameCollision parent, UndefinedWrapper<
            String

> value) {
    parent.type = value;
  }
  final PropertyReflection<PropertyNameCollision, UndefinedWrapper<
            String

>> type$Part;
  static UndefinedWrapper<
            String

> _type$Getter(PropertyNameCollision parent) {
    return parent.type$;
  }
  static void _type$Setter(PropertyNameCollision parent, UndefinedWrapper<
            String

> value) {
    parent.type$ = value;
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
  List<PropertyReflection<PropertyNameCollision, dynamic>> get properties => [
    $typePart,
typePart,
type$Part,
  ];

  final AdditionalPropertiesReflection<PropertyNameCollision, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<PropertyNameCollision, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<PropertyNameCollision, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PropertyNameCollision.canDeserialize(src);
  @override
  PropertyNameCollision Function(Object? src) get deserializeFunction =>
      (src) => PropertyNameCollision.deserialize(src);

  @override
  Object? Function(PropertyNameCollision src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of PropertyNameCollision.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  PropertyNameCollision example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = PropertyNameCollision(
      $type: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[$typePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      type: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[typePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      type$: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[type$Part.oasName]?.key.key;
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


class PropertyNameCollisionXmlReflection {
    const PropertyNameCollisionXmlReflection();
}

