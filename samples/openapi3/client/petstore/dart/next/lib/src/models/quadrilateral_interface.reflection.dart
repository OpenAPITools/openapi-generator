// Model reflection

part of 'quadrilateral_interface.dart';


//class reflection

class QuadrilateralInterfaceReflection extends ClassReflection<QuadrilateralInterface> {
  static QuadrilateralInterfaceReflection instanceGetter() => instance;
  static const instance = QuadrilateralInterfaceReflection._(
    modelName: r'QuadrilateralInterface',
    className: r'QuadrilateralInterface',
    quadrilateralTypePart: PropertyReflection<QuadrilateralInterface, 
            String
>(
      dartName: r'quadrilateralType',
      nullable: false,
      required: true,
      oasName: r'quadrilateralType',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _quadrilateralTypeGetter,
      setter: _quadrilateralTypeSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<QuadrilateralInterface, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const QuadrilateralInterfaceReflection._({
    required this.modelName,
    required this.className,
    required this.quadrilateralTypePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<QuadrilateralInterface, 
            String
> quadrilateralTypePart;
  static 
            String
 _quadrilateralTypeGetter(QuadrilateralInterface parent) {
    return parent.quadrilateralType;
  }
  static void _quadrilateralTypeSetter(QuadrilateralInterface parent, 
            String
 value) {
    parent.quadrilateralType = value;
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
  List<PropertyReflection<QuadrilateralInterface, dynamic>> get properties => [
    quadrilateralTypePart,
  ];

  final AdditionalPropertiesReflection<QuadrilateralInterface, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<QuadrilateralInterface, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<QuadrilateralInterface, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => QuadrilateralInterface.canDeserialize(src);
  @override
  QuadrilateralInterface Function(Object? src) get deserializeFunction =>
      (src) => QuadrilateralInterface.deserialize(src);

  @override
  Object? Function(QuadrilateralInterface src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of QuadrilateralInterface.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  QuadrilateralInterface example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return QuadrilateralInterface(
      quadrilateralType: () {
        PartReflection? _partReflection = _reflection.quadrilateralTypePart;
        
        final disc = discriminators[r'quadrilateralType'];
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

class QuadrilateralInterfaceXmlReflection {
    const QuadrilateralInterfaceXmlReflection();
}

