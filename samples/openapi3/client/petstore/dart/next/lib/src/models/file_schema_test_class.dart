// Model def

import 'package:petstore_api/_internal.dart';


part 'file_schema_test_class.reflection.dart';
part 'file_schema_test_class.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = FileSchemaTestClassReflection.instance;
  FileSchemaTestClassReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$FileSchemaTestClassToMap(this);
  }
  factory FileSchemaTestClass.fromMap(Map<String, dynamic> src) {
    return _$FileSchemaTestClassFromMap(src);
  }
  static FileSchemaTestClass? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return FileSchemaTestClass.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FileSchemaTestClassCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory FileSchemaTestClass.deserialize(Object? src) {
    return _$FileSchemaTestClassDeserialize(src);
  }
  static FileSchemaTestClass? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return FileSchemaTestClass.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FileSchemaTestClassCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$FileSchemaTestClassSerialize(this);
  }
}




