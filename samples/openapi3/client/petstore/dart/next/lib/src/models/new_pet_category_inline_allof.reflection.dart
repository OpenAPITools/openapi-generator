// Model reflection

part of 'new_pet_category_inline_allof.dart';


//class reflection

class NewPetCategoryInlineAllofReflection extends ClassReflection<NewPetCategoryInlineAllof> {
  static NewPetCategoryInlineAllofReflection instanceGetter() => instance;
  static const instance = NewPetCategoryInlineAllofReflection._(
    modelName: r'NewPet_category_inline_allof',
    className: r'NewPetCategoryInlineAllof',
    idPart: PropertyReflection<NewPetCategoryInlineAllof, UndefinedWrapper<
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
    namePart: PropertyReflection<NewPetCategoryInlineAllof, 
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
    categoryTagPart: PropertyReflection<NewPetCategoryInlineAllof, UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
>>(
      dartName: r'categoryTag',
      nullable: false,
      required: false,
      oasName: r'category_tag',
      oasType: r'NewPetCategoryInlineAllofAllOfCategoryTag',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: NewPetCategoryInlineAllofAllOfCategoryTagReflection.instance,
      getter: _categoryTagGetter,
      setter: _categoryTagSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<NewPetCategoryInlineAllof, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const NewPetCategoryInlineAllofReflection._({
    required this.modelName,
    required this.className,
    required this.idPart,
    required this.namePart,
    required this.categoryTagPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<NewPetCategoryInlineAllof, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(NewPetCategoryInlineAllof parent) {
    return parent.id;
  }
  static void _idSetter(NewPetCategoryInlineAllof parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }
  final PropertyReflection<NewPetCategoryInlineAllof, 
            String
> namePart;
  static 
            String
 _nameGetter(NewPetCategoryInlineAllof parent) {
    return parent.name;
  }
  static void _nameSetter(NewPetCategoryInlineAllof parent, 
            String
 value) {
    parent.name = value;
  }
  final PropertyReflection<NewPetCategoryInlineAllof, UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
>> categoryTagPart;
  static UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
> _categoryTagGetter(NewPetCategoryInlineAllof parent) {
    return parent.categoryTag;
  }
  static void _categoryTagSetter(NewPetCategoryInlineAllof parent, UndefinedWrapper<
            NewPetCategoryInlineAllofAllOfCategoryTag
> value) {
    parent.categoryTag = value;
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
  List<PropertyReflection<NewPetCategoryInlineAllof, dynamic>> get properties => [
    idPart,
namePart,
categoryTagPart,
  ];

  final AdditionalPropertiesReflection<NewPetCategoryInlineAllof, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<NewPetCategoryInlineAllof, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<NewPetCategoryInlineAllof, dynamic>> get allOfs => [
    
  ];

  @override
  List<OneOfReflection<NewPetCategoryInlineAllof, dynamic>> get oneOfs => [
    
  ];
  @override
  List<AnyOfReflection<NewPetCategoryInlineAllof, dynamic>> get anyOfs => [
    
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NewPetCategoryInlineAllof.canDeserialize(src);
  @override
  NewPetCategoryInlineAllof Function(Object? src) get deserializeFunction =>
      (src) => NewPetCategoryInlineAllof.deserialize(src);

  @override
  Object? Function(NewPetCategoryInlineAllof src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of NewPetCategoryInlineAllof.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  NewPetCategoryInlineAllof example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return NewPetCategoryInlineAllof(
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
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      categoryTag: () {
        PartReflection? _partReflection = _reflection.categoryTagPart;
        
        return UndefinedWrapper(


            
            


    NewPetCategoryInlineAllofAllOfCategoryTag.$reflection.example()
    


);
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class NewPetCategoryInlineAllofXmlReflection {
    const NewPetCategoryInlineAllofXmlReflection();
}

