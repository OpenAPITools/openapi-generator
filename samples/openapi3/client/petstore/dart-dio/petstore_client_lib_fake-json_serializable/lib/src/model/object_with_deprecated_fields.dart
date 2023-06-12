//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/deprecated_object.dart';
import 'package:json_annotation/json_annotation.dart';

part 'object_with_deprecated_fields.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ObjectWithDeprecatedFields {
  /// Returns a new [ObjectWithDeprecatedFields] instance.
  ObjectWithDeprecatedFields({

     this.uuid,

     this.id,

     this.deprecatedRef,

     this.bars,
  });

  @JsonKey(
    
    name: r'uuid',
    required: false,
    includeIfNull: false
  )


  final String? uuid;



  @Deprecated('id has been deprecated')
  @JsonKey(
    
    name: r'id',
    required: false,
    includeIfNull: false
  )


  final num? id;



  @Deprecated('deprecatedRef has been deprecated')
  @JsonKey(
    
    name: r'deprecatedRef',
    required: false,
    includeIfNull: false
  )


  final DeprecatedObject? deprecatedRef;



  @Deprecated('bars has been deprecated')
  @JsonKey(
    
    name: r'bars',
    required: false,
    includeIfNull: false
  )


  final List<String>? bars;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectWithDeprecatedFields &&
     other.uuid == uuid &&
     other.id == id &&
     other.deprecatedRef == deprecatedRef &&
     other.bars == bars;

  @override
  int get hashCode =>
    uuid.hashCode +
    id.hashCode +
    deprecatedRef.hashCode +
    bars.hashCode;

  factory ObjectWithDeprecatedFields.fromJson(Map<String, dynamic> json) => _$ObjectWithDeprecatedFieldsFromJson(json);

  Map<String, dynamic> toJson() => _$ObjectWithDeprecatedFieldsToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

