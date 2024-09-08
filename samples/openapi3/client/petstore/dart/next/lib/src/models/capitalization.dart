// Model def

import 'package:petstore_api/_internal.dart';


part 'capitalization.reflection.dart';


/// CapitalizationMixin
///
/// Properties:
/// * [smallCamel] 
/// * [capitalCamel] 
/// * [smallSnake] 
/// * [capitalSnake] 
/// * [scAETHFlowPoints] 
/// * [ATT_NAME] - Name of the pet 
mixin CapitalizationMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get smallCamel;
UndefinedWrapper<
            String
> get capitalCamel;
UndefinedWrapper<
            String
> get smallSnake;
UndefinedWrapper<
            String
> get capitalSnake;
UndefinedWrapper<
            String
> get scAETHFlowPoints;
UndefinedWrapper<
            String
> get ATT_NAME;
  
}

/// Capitalization
///
/// Properties:
/// * [smallCamel] 
/// * [capitalCamel] 
/// * [smallSnake] 
/// * [capitalSnake] 
/// * [scAETHFlowPoints] 
/// * [ATT_NAME] - Name of the pet 
class Capitalization with
$OpenApiObjectMixin,

CapitalizationMixin {
  @override
  UndefinedWrapper<
            String
> smallCamel;
  @override
  UndefinedWrapper<
            String
> capitalCamel;
  @override
  UndefinedWrapper<
            String
> smallSnake;
  @override
  UndefinedWrapper<
            String
> capitalSnake;
  @override
  UndefinedWrapper<
            String
> scAETHFlowPoints;
  @override
  UndefinedWrapper<
            String
> ATT_NAME;

  AdditionalProperties<Object
?> additionalProperties;

  

  Capitalization.$all({
        required this.smallCamel,
    required this.capitalCamel,
    required this.smallSnake,
    required this.capitalSnake,
    required this.scAETHFlowPoints,
    required this.ATT_NAME,
    required this.additionalProperties,
    
  });

  Capitalization({
      this.smallCamel = const UndefinedWrapper
        .undefined()
,
  this.capitalCamel = const UndefinedWrapper
        .undefined()
,
  this.smallSnake = const UndefinedWrapper
        .undefined()
,
  this.capitalSnake = const UndefinedWrapper
        .undefined()
,
  this.scAETHFlowPoints = const UndefinedWrapper
        .undefined()
,
  this.ATT_NAME = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = CapitalizationReflection.instance;
  CapitalizationReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Capitalization.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Capitalization clone() {
    return $reflection.clone(this);
  }
}




















