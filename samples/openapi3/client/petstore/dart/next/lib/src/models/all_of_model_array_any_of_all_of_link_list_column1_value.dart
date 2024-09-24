// Model def

import 'package:petstore_api/_internal.dart';


part 'all_of_model_array_any_of_all_of_link_list_column1_value.reflection.dart';


/// AllOfModelArrayAnyOfAllOfLinkListColumn1ValueMixin
///
/// Properties:
mixin AllOfModelArrayAnyOfAllOfLinkListColumn1ValueMixin on
  $OpenApiObjectMixin {
    
  UndefinedWrapper<
            User
> get anyOf0;
  UndefinedWrapper<
            Tag
> get anyOf1;
}

/// AllOfModelArrayAnyOfAllOfLinkListColumn1Value
///
/// Properties:
class AllOfModelArrayAnyOfAllOfLinkListColumn1Value with
$OpenApiObjectMixin,

AllOfModelArrayAnyOfAllOfLinkListColumn1ValueMixin {

  AdditionalProperties<Object
?> additionalProperties;

  
  @override
  UndefinedWrapper<
            User
> anyOf0;
  
  @override
  UndefinedWrapper<
            Tag
> anyOf1;
  

  AllOfModelArrayAnyOfAllOfLinkListColumn1Value.$all({
        required this.additionalProperties,
    
    required this.anyOf0,
    required this.anyOf1,
  });

  AllOfModelArrayAnyOfAllOfLinkListColumn1Value({
        AdditionalProperties<Object
?>? additionalProperties,
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection.instance;
  AllOfModelArrayAnyOfAllOfLinkListColumn1ValueReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory AllOfModelArrayAnyOfAllOfLinkListColumn1Value.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  AllOfModelArrayAnyOfAllOfLinkListColumn1Value clone() {
    return $reflection.clone(this);
  }
}


