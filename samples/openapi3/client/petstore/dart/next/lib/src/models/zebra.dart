// Model def

import 'package:petstore_api/_internal.dart';


part 'zebra.reflection.dart';


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
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = ZebraReflection.instance;
  ZebraReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Zebra.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Zebra clone() {
    return $reflection.clone(this);
  }
}


extension type const ZebraTypeEnum._(String value) implements String {
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

  static const $reflection = EnumReflection<ZebraTypeEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'plains', oasValue: r'plains', value: ZebraTypeEnum.plains()),
      
        EnumMemberReflection(dartName: r'mountain', oasValue: r'mountain', value: ZebraTypeEnum.mountain()),
      
        EnumMemberReflection(dartName: r'grevys', oasValue: r'grevys', value: ZebraTypeEnum.grevys()),
      
    ],
  );

  factory ZebraTypeEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
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





