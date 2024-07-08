// Model reflection

part of 'shape_interface.dart';


//class reflection

class ShapeInterfaceReflection extends ClassReflection<ShapeInterface> {
  static ShapeInterfaceReflection instanceGetter() => instance;
  static const instance = ShapeInterfaceReflection._(
    modelName: r'ShapeInterface',
    className: r'ShapeInterface',
    shapeTypePart: PropertyReflection<ShapeInterface, 
            String

>(
      dartName: r'shapeType',
      nullable: false,
      required: true,
      oasName: r'shapeType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _shapeTypeGetter,
      setter: _shapeTypeSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<ShapeInterface, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const ShapeInterfaceReflection._({
    required this.modelName,
    required this.className,
    required this.shapeTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<ShapeInterface, 
            String

> shapeTypePart;
  static 
            String

 _shapeTypeGetter(ShapeInterface parent) {
    return parent.shapeType;
  }
  static void _shapeTypeSetter(ShapeInterface parent, 
            String

 value) {
    parent.shapeType = value;
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
  List<PropertyReflection<ShapeInterface, dynamic>> get properties => [
    shapeTypePart,
  ];

  final AdditionalPropertiesReflection<ShapeInterface, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<ShapeInterface, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<ShapeInterface, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => ShapeInterface.canDeserialize(src);
  @override
  ShapeInterface Function(Object? src) get deserializeFunction =>
      (src) => ShapeInterface.deserialize(src);

  @override
  Object? Function(ShapeInterface src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of ShapeInterface.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  ShapeInterface example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = ShapeInterface(
      shapeType: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[shapeTypePart.oasName]?.key.key;
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


class ShapeInterfaceXmlReflection {
    const ShapeInterfaceXmlReflection();
}

