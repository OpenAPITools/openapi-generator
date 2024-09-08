// Model def

import 'package:petstore_api/_internal.dart';


part 'file.reflection.dart';


/// Must be named `File` for test.
///
/// Properties:
/// * [sourceURI] - Test capitalization
mixin FileMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get sourceURI;
  
}

/// Must be named `File` for test.
///
/// Properties:
/// * [sourceURI] - Test capitalization
class File with
$OpenApiObjectMixin,

FileMixin {
  @override
  UndefinedWrapper<
            String
> sourceURI;

  AdditionalProperties<Object
?> additionalProperties;

  

  File.$all({
        required this.sourceURI,
    required this.additionalProperties,
    
  });

  File({
      this.sourceURI = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = FileReflection.instance;
  FileReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory File.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  File clone() {
    return $reflection.clone(this);
  }
}





