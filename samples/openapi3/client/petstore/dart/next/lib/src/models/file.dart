// Model def

import 'package:openapi/_internal.dart';


part 'file.reflection.dart';
part 'file.serialization.dart';


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

  

  

  File.$all({
        required this.sourceURI,
    
    
  });

  File({
      this.sourceURI = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = FileReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$FileToMap(this);
  }
  factory File.fromMap(Map<String, dynamic> src) {
    return _$FileFromMap(src);
  }
  static File? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return File.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FileCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory File.deserialize(Object? src) {
    return _$FileDeserialize(src);
  }
  static File? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return File.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FileCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$FileSerialize(this);
  }
}




