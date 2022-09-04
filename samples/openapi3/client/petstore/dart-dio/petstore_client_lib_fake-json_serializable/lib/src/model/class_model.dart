//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'class_model.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ClassModel {
  /// Returns a new [ClassModel] instance.
  ClassModel({

     this.class_,
  });

  @JsonKey(
    
    name: r'_class',
    required: false,
    includeIfNull: false
  )


  final String? class_;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ClassModel &&
     other.class_ == class_;

  @override
  int get hashCode =>
    class_.hashCode;

  factory ClassModel.fromJson(Map<String, dynamic> json) => _$ClassModelFromJson(json);

  Map<String, dynamic> toJson() => _$ClassModelToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

