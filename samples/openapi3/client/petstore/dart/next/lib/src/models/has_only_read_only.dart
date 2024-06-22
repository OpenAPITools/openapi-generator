// Model def

import 'package:openapi/_internal.dart';


part 'has_only_read_only.reflection.dart';
part 'has_only_read_only.serialization.dart';


/// HasOnlyReadOnlyMixin
///
/// Properties:
/// * [bar] 
/// * [foo] 
mixin HasOnlyReadOnlyMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get bar;
UndefinedWrapper<
            String
> get foo;
  
}

/// HasOnlyReadOnly
///
/// Properties:
/// * [bar] 
/// * [foo] 
class HasOnlyReadOnly with
$OpenApiObjectMixin,


HasOnlyReadOnlyMixin {
  @override
  UndefinedWrapper<
            String
> bar;
  @override
  UndefinedWrapper<
            String
> foo;

  

  

  HasOnlyReadOnly.$all({
        required this.bar,
    required this.foo,
    
    
  });

  HasOnlyReadOnly({
      this.bar = const UndefinedWrapper
        .undefined()
,
  this.foo = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = HasOnlyReadOnlyReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$HasOnlyReadOnlyToMap(this);
  }
  factory HasOnlyReadOnly.fromMap(Map<String, dynamic> src) {
    return _$HasOnlyReadOnlyFromMap(src);
  }
  static HasOnlyReadOnly? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return HasOnlyReadOnly.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$HasOnlyReadOnlyCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory HasOnlyReadOnly.deserialize(Object? src) {
    return _$HasOnlyReadOnlyDeserialize(src);
  }
  static HasOnlyReadOnly? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return HasOnlyReadOnly.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$HasOnlyReadOnlyCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$HasOnlyReadOnlySerialize(this);
  }
}




