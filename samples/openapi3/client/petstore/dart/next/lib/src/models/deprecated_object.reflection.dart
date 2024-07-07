// Model reflection

part of 'deprecated_object.dart';


//class reflection

class DeprecatedObjectReflection extends ClassReflection<DeprecatedObject> {
  static DeprecatedObjectReflection instanceGetter() => instance;
  static const instance = DeprecatedObjectReflection._(
    modelName: r'DeprecatedObject',
    className: r'DeprecatedObject',
    namePart: PropertyReflection<DeprecatedObject, UndefinedWrapper<
            String
>>(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _nameGetter,
      setter: _nameSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<DeprecatedObject, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const DeprecatedObjectReflection._({
    required this.modelName,
    required this.className,
    required this.namePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<DeprecatedObject, UndefinedWrapper<
            String
>> namePart;
  static UndefinedWrapper<
            String
> _nameGetter(DeprecatedObject parent) {
    return parent.name;
  }
  static void _nameSetter(DeprecatedObject parent, UndefinedWrapper<
            String
> value) {
    parent.name = value;
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
  List<PropertyReflection<DeprecatedObject, dynamic>> get properties => [
    namePart,
  ];

  final AdditionalPropertiesReflection<DeprecatedObject, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<DeprecatedObject, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<DeprecatedObject, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => DeprecatedObject.canDeserialize(src);
  @override
  DeprecatedObject Function(Object? src) get deserializeFunction =>
      (src) => DeprecatedObject.deserialize(src);

  @override
  Object? Function(DeprecatedObject src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of DeprecatedObject.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  DeprecatedObject example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return DeprecatedObject(
      name: () {
        PartReflection? _partReflection = _reflection.namePart;
        
        final disc = discriminators[r'name'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return UndefinedWrapper(result);
          }
        }
        
        return UndefinedWrapper(


            
            


    
    exampleString()


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class DeprecatedObjectXmlReflection {
    const DeprecatedObjectXmlReflection();
}

