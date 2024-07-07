// Model reflection

part of 'new_pet_category_inline_allof_all_of_category_tag.dart';


//class reflection

class NewPetCategoryInlineAllofAllOfCategoryTagReflection extends ClassReflection<NewPetCategoryInlineAllofAllOfCategoryTag> {
  static NewPetCategoryInlineAllofAllOfCategoryTagReflection instanceGetter() => instance;
  static const instance = NewPetCategoryInlineAllofAllOfCategoryTagReflection._(
    modelName: r'NewPet_category_inline_allof_allOf_category_tag',
    className: r'NewPetCategoryInlineAllofAllOfCategoryTag',
    idPart: PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, UndefinedWrapper<
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
    namePart: PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, UndefinedWrapper<
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
      itemsReflection: ItemsReflection<NewPetCategoryInlineAllofAllOfCategoryTag, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const NewPetCategoryInlineAllofAllOfCategoryTagReflection._({
    required this.modelName,
    required this.className,
    required this.idPart,
    required this.namePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(NewPetCategoryInlineAllofAllOfCategoryTag parent) {
    return parent.id;
  }
  static void _idSetter(NewPetCategoryInlineAllofAllOfCategoryTag parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }
  final PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, UndefinedWrapper<
            String
>> namePart;
  static UndefinedWrapper<
            String
> _nameGetter(NewPetCategoryInlineAllofAllOfCategoryTag parent) {
    return parent.name;
  }
  static void _nameSetter(NewPetCategoryInlineAllofAllOfCategoryTag parent, UndefinedWrapper<
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
  List<PropertyReflection<NewPetCategoryInlineAllofAllOfCategoryTag, dynamic>> get properties => [
    idPart,
namePart,
  ];

  final AdditionalPropertiesReflection<NewPetCategoryInlineAllofAllOfCategoryTag, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<NewPetCategoryInlineAllofAllOfCategoryTag, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<NewPetCategoryInlineAllofAllOfCategoryTag, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NewPetCategoryInlineAllofAllOfCategoryTag.canDeserialize(src);
  @override
  NewPetCategoryInlineAllofAllOfCategoryTag Function(Object? src) get deserializeFunction =>
      (src) => NewPetCategoryInlineAllofAllOfCategoryTag.deserialize(src);

  @override
  Object? Function(NewPetCategoryInlineAllofAllOfCategoryTag src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of NewPetCategoryInlineAllofAllOfCategoryTag.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  NewPetCategoryInlineAllofAllOfCategoryTag example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return NewPetCategoryInlineAllofAllOfCategoryTag(
      id: () {
        PartReflection? _partReflection = _reflection.idPart;
        
        return UndefinedWrapper(


            
            


    
    exampleint()


);
      }(),
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

class NewPetCategoryInlineAllofAllOfCategoryTagXmlReflection {
    const NewPetCategoryInlineAllofAllOfCategoryTagXmlReflection();
}

