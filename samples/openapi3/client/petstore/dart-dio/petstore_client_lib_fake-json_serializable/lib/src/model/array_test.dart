//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/read_only_first.dart';
import 'package:json_annotation/json_annotation.dart';

part 'array_test.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ArrayTest {
  /// Returns a new [ArrayTest] instance.
  ArrayTest({

     this.arrayOfString,

     this.arrayArrayOfInteger,

     this.arrayArrayOfModel,
  });

  @JsonKey(
    
    name: r'array_of_string',
    required: false,
    includeIfNull: false
  )


  final List<String>? arrayOfString;



  @JsonKey(
    
    name: r'array_array_of_integer',
    required: false,
    includeIfNull: false
  )


  final List<List<int>>? arrayArrayOfInteger;



  @JsonKey(
    
    name: r'array_array_of_model',
    required: false,
    includeIfNull: false
  )


  final List<List<ReadOnlyFirst>>? arrayArrayOfModel;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ArrayTest &&
     other.arrayOfString == arrayOfString &&
     other.arrayArrayOfInteger == arrayArrayOfInteger &&
     other.arrayArrayOfModel == arrayArrayOfModel;

  @override
  int get hashCode =>
    arrayOfString.hashCode +
    arrayArrayOfInteger.hashCode +
    arrayArrayOfModel.hashCode;

  factory ArrayTest.fromJson(Map<String, dynamic> json) => _$ArrayTestFromJson(json);

  Map<String, dynamic> toJson() => _$ArrayTestToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

