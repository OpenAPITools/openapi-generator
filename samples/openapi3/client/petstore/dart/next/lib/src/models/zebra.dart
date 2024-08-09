// Model def

import 'package:petstore_api/_internal.dart';


part 'zebra.reflection.dart';
part 'zebra.serialization.dart';


/// ZebraMixin
///
/// Properties:
/// * [type] 
/// * [className] 
mixin ZebraMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            ZebraTypeEnum

> get type;

            String

 get className;
  
}

/// Zebra
///
/// Properties:
/// * [type] 
/// * [className] 
class Zebra with
$OpenApiObjectMixin,

ZebraMixin {
  @override
  UndefinedWrapper<
            ZebraTypeEnum

> type;
  @override
  
            String

 className;

  AdditionalProperties<Object

?> additionalProperties;

  

  Zebra.$all({
        required this.type,
    required this.className,
    required this.additionalProperties,
    
  });

  Zebra({
      this.type = const UndefinedWrapper
        .undefined()
,
required  this.className     ,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ZebraReflection.instance;
  ZebraReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$ZebraToMap(this);
  }
  factory Zebra.fromMap(Map<String, dynamic> src) {
    return _$ZebraFromMap(src);
  }
  static Zebra? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Zebra.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ZebraCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Zebra.deserialize(Object? src) {
    return _$ZebraDeserialize(src);
  }
  static Zebra? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Zebra.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ZebraCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ZebraSerialize(this);
  }
}




extension type const ZebraTypeEnum._(String value) {
      const ZebraTypeEnum.plains() : this._(r'plains');
      const ZebraTypeEnum.mountain() : this._(r'mountain');
      const ZebraTypeEnum.grevys() : this._(r'grevys');

  /// Creates a [ZebraTypeEnum] enum from a value and safely checking if it exists.
  factory ZebraTypeEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static bool canDeserialize(Object? value) {
    return value is String && values.where((element) => element.value == value).firstOrNull != null;
  }

  /// Creates a [ZebraTypeEnum] enum from a value without checking if it exists.
  const ZebraTypeEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<ZebraTypeEnum> values = [
    ZebraTypeEnum.plains(),
    ZebraTypeEnum.mountain(),
    ZebraTypeEnum.grevys(),
    
  ];
}

