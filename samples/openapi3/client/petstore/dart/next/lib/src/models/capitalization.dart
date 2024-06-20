// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

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
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get smallCamel;
  UndefinedWrapper<String> get capitalCamel;
  UndefinedWrapper<String> get smallSnake;
  UndefinedWrapper<String> get capitalSnake;
  UndefinedWrapper<String> get scAETHFlowPoints;
  UndefinedWrapper<String> get ATT_NAME;

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
  UndefinedWrapper<String> smallCamel;
  @override
  UndefinedWrapper<String> capitalCamel;
  @override
  UndefinedWrapper<String> smallSnake;
  @override
  UndefinedWrapper<String> capitalSnake;
  @override
  UndefinedWrapper<String> scAETHFlowPoints;
  @override
  UndefinedWrapper<String> ATT_NAME;





  Capitalization.$all({
    required this.smallCamel,
    required this.capitalCamel,
    required this.smallSnake,
    required this.capitalSnake,
    required this.scAETHFlowPoints,
    required this.ATT_NAME,
    
    
  });

  Capitalization({
    this.smallCamel = const UndefinedWrapper.undefined(),
    this.capitalCamel = const UndefinedWrapper.undefined(),
    this.smallSnake = const UndefinedWrapper.undefined(),
    this.capitalSnake = const UndefinedWrapper.undefined(),
    this.scAETHFlowPoints = const UndefinedWrapper.undefined(),
    this.ATT_NAME = const UndefinedWrapper.undefined(),
    
    
  });
}




