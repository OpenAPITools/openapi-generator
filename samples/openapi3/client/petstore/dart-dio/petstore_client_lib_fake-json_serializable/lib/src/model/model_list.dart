//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'model_list.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ModelList {
  /// Returns a new [ModelList] instance.
  ModelList({

     this.n123list,
  });

  @JsonKey(
    
    name: r'123-list',
    required: false,
    includeIfNull: false
  )


  final String? n123list;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelList &&
     other.n123list == n123list;

  @override
  int get hashCode =>
    n123list.hashCode;

  factory ModelList.fromJson(Map<String, dynamic> json) => _$ModelListFromJson(json);

  Map<String, dynamic> toJson() => _$ModelListToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

