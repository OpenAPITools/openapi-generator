//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/object_with_duplicate_inline_enum.dart';
import 'package:openapi/src/model/object_with_inline_enum.dart';
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'object_that_references_objects_with_duplicate_inline_enums.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ObjectThatReferencesObjectsWithDuplicateInlineEnums {
  /// Returns a new [ObjectThatReferencesObjectsWithDuplicateInlineEnums] instance.
  ObjectThatReferencesObjectsWithDuplicateInlineEnums({

     this.objectOne,

     this.objectTwo,
  });

  @JsonKey(
    
    name: r'object_one',
    required: false,
    includeIfNull: false,
  )


  final ObjectWithInlineEnum? objectOne;



  @JsonKey(
    
    name: r'object_two',
    required: false,
    includeIfNull: false,
  )


  final ObjectWithDuplicateInlineEnum? objectTwo;





    @override
    bool operator ==(Object other) => identical(this, other) || other is ObjectThatReferencesObjectsWithDuplicateInlineEnums &&
      other.objectOne == objectOne &&
      other.objectTwo == objectTwo;

    @override
    int get hashCode =>
        objectOne.hashCode +
        objectTwo.hashCode;

  factory ObjectThatReferencesObjectsWithDuplicateInlineEnums.fromJson(Map<String, dynamic> json) => _$ObjectThatReferencesObjectsWithDuplicateInlineEnumsFromJson(json);

  Map<String, dynamic> toJson() => _$ObjectThatReferencesObjectsWithDuplicateInlineEnumsToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

