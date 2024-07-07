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
  ShapeInterface example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return ShapeInterface(
      shapeType: () {
        PartReflection? _partReflection = _reflection.shapeTypePart;
        
        final disc = discriminators[r'shapeType'];
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

class ShapeInterfaceXmlReflection {
    const ShapeInterfaceXmlReflection();
}

