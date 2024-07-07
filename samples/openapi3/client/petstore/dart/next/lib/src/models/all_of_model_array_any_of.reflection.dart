// Model reflection

part of 'all_of_model_array_any_of.dart';


//class reflection

class AllOfModelArrayAnyOfReflection extends ClassReflection<AllOfModelArrayAnyOf> {
  static AllOfModelArrayAnyOfReflection instanceGetter() => instance;
  static const instance = AllOfModelArrayAnyOfReflection._(
    modelName: r'AllOfModelArrayAnyOf',
    className: r'AllOfModelArrayAnyOf',
    namePart: PropertyReflection<AllOfModelArrayAnyOf, 
            String
>(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _nameGetter,
      setter: _nameSetter,
    ),
    attributesPart: PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
>>(
      dartName: r'attributes',
      nullable: false,
      required: false,
      oasName: r'attributes',
      oasType: r'AllOfModelArrayAnyOfAllOfAttributes',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: AllOfModelArrayAnyOfAllOfAttributesReflection.instance,
      getter: _attributesGetter,
      setter: _attributesSetter,
    ),
    idPart: PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            int
>>(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _idGetter,
      setter: _idSetter,
    ),
    linkListColumn1Part: PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
>>(
      dartName: r'linkListColumn1',
      nullable: false,
      required: false,
      oasName: r'linkListColumn1',
      oasType: r'AllOfModelArrayAnyOfAllOfLinkListColumn1',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection.instance,
      getter: _linkListColumn1Getter,
      setter: _linkListColumn1Setter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<AllOfModelArrayAnyOf, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const AllOfModelArrayAnyOfReflection._({
    required this.modelName,
    required this.className,
    required this.namePart,
    required this.attributesPart,
    required this.idPart,
    required this.linkListColumn1Part,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<AllOfModelArrayAnyOf, 
            String
> namePart;
  static 
            String
 _nameGetter(AllOfModelArrayAnyOf parent) {
    return parent.name;
  }
  static void _nameSetter(AllOfModelArrayAnyOf parent, 
            String
 value) {
    parent.name = value;
  }
  final PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
>> attributesPart;
  static UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
> _attributesGetter(AllOfModelArrayAnyOf parent) {
    return parent.attributes;
  }
  static void _attributesSetter(AllOfModelArrayAnyOf parent, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
> value) {
    parent.attributes = value;
  }
  final PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(AllOfModelArrayAnyOf parent) {
    return parent.id;
  }
  static void _idSetter(AllOfModelArrayAnyOf parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }
  final PropertyReflection<AllOfModelArrayAnyOf, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
>> linkListColumn1Part;
  static UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
> _linkListColumn1Getter(AllOfModelArrayAnyOf parent) {
    return parent.linkListColumn1;
  }
  static void _linkListColumn1Setter(AllOfModelArrayAnyOf parent, UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
> value) {
    parent.linkListColumn1 = value;
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
  List<PropertyReflection<AllOfModelArrayAnyOf, dynamic>> get properties => [
    namePart,
attributesPart,
idPart,
linkListColumn1Part,
  ];

  final AdditionalPropertiesReflection<AllOfModelArrayAnyOf, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<AllOfModelArrayAnyOf, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<AllOfModelArrayAnyOf, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<AllOfModelArrayAnyOf, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<AllOfModelArrayAnyOf, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => AllOfModelArrayAnyOf.canDeserialize(src);
  @override
  AllOfModelArrayAnyOf Function(Object? src) get deserializeFunction =>
      (src) => AllOfModelArrayAnyOf.deserialize(src);

  @override
  Object? Function(AllOfModelArrayAnyOf src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of AllOfModelArrayAnyOf.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  AllOfModelArrayAnyOf example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return AllOfModelArrayAnyOf(
      name: () {
        PartReflection? _partReflection = _reflection.namePart;
        
        final disc = discriminators[r'name'];
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
      attributes: () {
        PartReflection? _partReflection = _reflection.attributesPart;
        
        return UndefinedWrapper(


            
            


    AllOfModelArrayAnyOfAllOfAttributes.$reflection.example()
    


);
      }(),
      id: () {
        PartReflection? _partReflection = _reflection.idPart;
        
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
      linkListColumn1: () {
        PartReflection? _partReflection = _reflection.linkListColumn1Part;
        
        return UndefinedWrapper(


            
            


    AllOfModelArrayAnyOfAllOfLinkListColumn1.$reflection.example()
    


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class AllOfModelArrayAnyOfXmlReflection {
    const AllOfModelArrayAnyOfXmlReflection();
}

