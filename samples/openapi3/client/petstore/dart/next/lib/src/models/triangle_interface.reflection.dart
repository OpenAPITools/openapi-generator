// Model reflection

part of 'triangle_interface.dart';


//class reflection

class TriangleInterfaceReflection extends ClassReflection<TriangleInterface> {
  static TriangleInterfaceReflection instanceGetter() => instance;
  static const instance = TriangleInterfaceReflection._(
    modelName: r'TriangleInterface',
    className: r'TriangleInterface',
    triangleTypePart: PropertyReflection<TriangleInterface, 
            String

>(
      dartName: r'triangleType',
      nullable: false,
      required: true,
      oasName: r'triangleType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _triangleTypeGetter,
      setter: _triangleTypeSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<TriangleInterface, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const TriangleInterfaceReflection._({
    required this.modelName,
    required this.className,
    required this.triangleTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<TriangleInterface, 
            String

> triangleTypePart;
  static 
            String

 _triangleTypeGetter(TriangleInterface parent) {
    return parent.triangleType;
  }
  static void _triangleTypeSetter(TriangleInterface parent, 
            String

 value) {
    parent.triangleType = value;
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
  List<PropertyReflection<TriangleInterface, dynamic>> get properties => [
    triangleTypePart,
  ];

  final AdditionalPropertiesReflection<TriangleInterface, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<TriangleInterface, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<TriangleInterface, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => TriangleInterface.canDeserialize(src);
  @override
  TriangleInterface Function(Object? src) get deserializeFunction =>
      (src) => TriangleInterface.deserialize(src);

  @override
  Object? Function(TriangleInterface src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of TriangleInterface.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  TriangleInterface example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = TriangleInterface(
      triangleType: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[triangleTypePart.oasName]?.key.key;
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


class TriangleInterfaceXmlReflection {
    const TriangleInterfaceXmlReflection();
}

