// Model def

import 'package:petstore_api/_internal.dart';


part 'category.reflection.dart';


/// CategoryMixin
///
/// Properties:
/// * [id] 
/// * [name] 
mixin CategoryMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            int
> get id;

            String
 get name;
  
}

/// Category
///
/// Properties:
/// * [id] 
/// * [name] 
class Category with
$OpenApiObjectMixin,

CategoryMixin {
  @override
  UndefinedWrapper<
            int
> id;
  @override
  
            String
 name;

  AdditionalProperties<Object
?> additionalProperties;

  

  Category.$all({
        required this.id,
    required this.name,
    required this.additionalProperties,
    
  });

  Category({
      this.id = const UndefinedWrapper
        .undefined()
,
  this.name     =
        
        'default-name'
        
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = CategoryReflection.instance;
  CategoryReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Category.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Category clone() {
    return $reflection.clone(this);
  }
}








