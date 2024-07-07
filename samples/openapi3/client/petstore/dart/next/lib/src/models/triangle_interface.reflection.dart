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
  TriangleInterface example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return TriangleInterface(
      triangleType: () {
        PartReflection? _partReflection = _reflection.triangleTypePart;
        
        final disc = discriminators[r'triangleType'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class TriangleInterfaceXmlReflection {
    const TriangleInterfaceXmlReflection();
}

