// Model def

import 'package:openapi/_internal.dart';


part 'capitalization.reflection.dart';
part 'capitalization.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = CapitalizationReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$CapitalizationToMap(this);
  }
  factory Capitalization.fromMap(Map<String, dynamic> src) {
    return _$CapitalizationFromMap(src);
  }
  static Capitalization? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Capitalization.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$CapitalizationCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Capitalization.deserialize(Object? src) {
    return _$CapitalizationDeserialize(src);
  }
  static Capitalization? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Capitalization.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$CapitalizationCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$CapitalizationSerialize(this);
  }
}




