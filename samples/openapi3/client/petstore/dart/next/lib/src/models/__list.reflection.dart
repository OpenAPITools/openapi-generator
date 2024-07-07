// Model reflection

part of '__list.dart';


//class reflection

class $ListReflection extends ClassReflection<$List> {
  static $ListReflection instanceGetter() => instance;
  static const instance = $ListReflection._(
    modelName: r'List',
    className: r'$List',
    $123listPart: PropertyReflection<$List, UndefinedWrapper<
            String
>>(
      dartName: r'$123list',
      nullable: false,
      required: false,
      oasName: r'123-list',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _$123listGetter,
      setter: _$123listSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<$List, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const $ListReflection._({
    required this.modelName,
    required this.className,
    required this.$123listPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<$List, UndefinedWrapper<
            String
>> $123listPart;
  static UndefinedWrapper<
            String
> _$123listGetter($List parent) {
    return parent.$123list;
  }
  static void _$123listSetter($List parent, UndefinedWrapper<
            String
> value) {
    parent.$123list = value;
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
  List<PropertyReflection<$List, dynamic>> get properties => [
    $123listPart,
  ];

  final AdditionalPropertiesReflection<$List, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<$List, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<$List, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => $List.canDeserialize(src);
  @override
  $List Function(Object? src) get deserializeFunction =>
      (src) => $List.deserialize(src);

  @override
  Object? Function($List src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of $List.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  $List example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return $List(
      $123list: () {
        PartReflection? _partReflection = _reflection.$123listPart;
        
        final disc = discriminators[r'123-list'];
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

class $ListXmlReflection {
    const $ListXmlReflection();
}

