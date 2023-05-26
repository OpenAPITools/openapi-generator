//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/single_ref_type.dart';
import 'package:json_annotation/json_annotation.dart';

part 'all_of_with_single_ref.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class AllOfWithSingleRef {
  /// Returns a new [AllOfWithSingleRef] instance.
  AllOfWithSingleRef({

     this.username,

     this.singleRefType,
  });

  @JsonKey(
    
    name: r'username',
    required: false,
    includeIfNull: false
  )


  final String? username;



  @JsonKey(
    
    name: r'SingleRefType',
    required: false,
    includeIfNull: false
  )


  final SingleRefType? singleRefType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is AllOfWithSingleRef &&
     other.username == username &&
     other.singleRefType == singleRefType;

  @override
  int get hashCode =>
    username.hashCode +
    singleRefType.hashCode;

  factory AllOfWithSingleRef.fromJson(Map<String, dynamic> json) => _$AllOfWithSingleRefFromJson(json);

  Map<String, dynamic> toJson() => _$AllOfWithSingleRefToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

