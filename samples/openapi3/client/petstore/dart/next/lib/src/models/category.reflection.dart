// Model reflection

part of 'category.dart';


//class reflection

class CategoryReflection extends ClassReflection<Category> {
  static CategoryReflection instanceGetter() => instance;
  static const instance = CategoryReflection._(
    modelName: r'Category',
    className: r'Category',
    idPart: PropertyReflection<Category, UndefinedWrapper<
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
    namePart: PropertyReflection<Category, 
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
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Category, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const CategoryReflection._({
    required this.modelName,
    required this.className,
    required this.idPart,
    required this.namePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Category, UndefinedWrapper<
            int
>> idPart;
  static UndefinedWrapper<
            int
> _idGetter(Category parent) {
    return parent.id;
  }
  static void _idSetter(Category parent, UndefinedWrapper<
            int
> value) {
    parent.id = value;
  }
  final PropertyReflection<Category, 
            String
> namePart;
  static 
            String
 _nameGetter(Category parent) {
    return parent.name;
  }
  static void _nameSetter(Category parent, 
            String
 value) {
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
  List<PropertyReflection<Category, dynamic>> get properties => [
    idPart,
namePart,
  ];

  final AdditionalPropertiesReflection<Category, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Category, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Category, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Category.canDeserialize(src);
  @override
  Category Function(Object? src) get deserializeFunction =>
      (src) => Category.deserialize(src);

  @override
  Object? Function(Category src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of Category.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Category example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Category(
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
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class CategoryXmlReflection {
    const CategoryXmlReflection();
}

