// Model def

import 'package:petstore_api/_internal.dart';


part 'file_schema_test_class.reflection.dart';


/// FileSchemaTestClassMixin
///
/// Properties:
/// * [file] 
/// * [files] 
mixin FileSchemaTestClassMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            File
> get file;
UndefinedWrapper<
    List<
        
            File
>
> get files;
  
}

/// FileSchemaTestClass
///
/// Properties:
/// * [file] 
/// * [files] 
class FileSchemaTestClass with
$OpenApiObjectMixin,

FileSchemaTestClassMixin {
  @override
  UndefinedWrapper<
            File
> file;
  @override
  UndefinedWrapper<
    List<
        
            File
>
> files;

  AdditionalProperties<Object
?> additionalProperties;

  

  FileSchemaTestClass.$all({
        required this.file,
    required this.files,
    required this.additionalProperties,
    
  });

  FileSchemaTestClass({
      this.file = const UndefinedWrapper
        .undefined()
,
  this.files = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = FileSchemaTestClassReflection.instance;
  FileSchemaTestClassReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory FileSchemaTestClass.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  FileSchemaTestClass clone() {
    return $reflection.clone(this);
  }
}










