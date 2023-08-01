//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'cat_all_of.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class CatAllOf {
  /// Returns a new [CatAllOf] instance.
  CatAllOf({

     this.declawed,
  });

  @JsonKey(
    
    name: r'declawed',
    required: false,
    includeIfNull: false
  )


  final bool? declawed;



  @override
  bool operator ==(Object other) => identical(this, other) || other is CatAllOf &&
     other.declawed == declawed;

  @override
  int get hashCode =>
    declawed.hashCode;

  factory CatAllOf.fromJson(Map<String, dynamic> json) => _$CatAllOfFromJson(json);

  Map<String, dynamic> toJson() => _$CatAllOfToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

