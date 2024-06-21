// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'test_endpoint_parameters_request.reflection.dart';
part 'test_endpoint_parameters_request.serialization.dart';


/// TestEndpointParametersRequestMixin
///
/// Properties:
/// * [integer] - None
/// * [int32] - None
/// * [int64] - None
/// * [number] - None
/// * [float] - None
/// * [$double] - None
/// * [string] - None
/// * [patternWithoutDelimiter] - None
/// * [byte] - None
/// * [binary] - None
/// * [date] - None
/// * [dateTime] - None
/// * [password] - None
/// * [callback] - None
mixin TestEndpointParametersRequestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<int> get integer;
  UndefinedWrapper<int> get int32;
  UndefinedWrapper<int> get int64;
  num get number;
  UndefinedWrapper<double> get float;
  double get $double;
  UndefinedWrapper<String> get string;
  String get patternWithoutDelimiter;
  Uint8List get byte;
  UndefinedWrapper<MyFile> get binary;
  UndefinedWrapper<DateTime> get date;
  UndefinedWrapper<DateTime> get dateTime;
  UndefinedWrapper<String> get password;
  UndefinedWrapper<String> get callback;

}

/// TestEndpointParametersRequest
///
/// Properties:
/// * [integer] - None
/// * [int32] - None
/// * [int64] - None
/// * [number] - None
/// * [float] - None
/// * [$double] - None
/// * [string] - None
/// * [patternWithoutDelimiter] - None
/// * [byte] - None
/// * [binary] - None
/// * [date] - None
/// * [dateTime] - None
/// * [password] - None
/// * [callback] - None
class TestEndpointParametersRequest with
$OpenApiObjectMixin,


TestEndpointParametersRequestMixin {
  @override
  UndefinedWrapper<int> integer;
  @override
  UndefinedWrapper<int> int32;
  @override
  UndefinedWrapper<int> int64;
  @override
  num number;
  @override
  UndefinedWrapper<double> float;
  @override
  double $double;
  @override
  UndefinedWrapper<String> string;
  @override
  String patternWithoutDelimiter;
  @override
  Uint8List byte;
  @override
  UndefinedWrapper<MyFile> binary;
  @override
  UndefinedWrapper<DateTime> date;
  @override
  UndefinedWrapper<DateTime> dateTime;
  @override
  UndefinedWrapper<String> password;
  @override
  UndefinedWrapper<String> callback;





  TestEndpointParametersRequest.$all({
    required this.integer,
    required this.int32,
    required this.int64,
    required this.number,
    required this.float,
    required this.$double,
    required this.string,
    required this.patternWithoutDelimiter,
    required this.byte,
    required this.binary,
    required this.date,
    required this.dateTime,
    required this.password,
    required this.callback,
    
    
  });

  TestEndpointParametersRequest({
    this.integer = const UndefinedWrapper.undefined(),
    this.int32 = const UndefinedWrapper.undefined(),
    this.int64 = const UndefinedWrapper.undefined(),
  required  this.number ,
    this.float = const UndefinedWrapper.undefined(),
  required  this.$double ,
    this.string = const UndefinedWrapper.undefined(),
  required  this.patternWithoutDelimiter ,
  required  this.byte ,
    this.binary = const UndefinedWrapper.undefined(),
    this.date = const UndefinedWrapper.undefined(),
    this.dateTime = const UndefinedWrapper.undefined(),
    this.password = const UndefinedWrapper.undefined(),
    this.callback = const UndefinedWrapper.undefined(),
    
    
  });
}




