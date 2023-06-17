//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'banana_req_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class BananaReqDisc {
  /// Returns a new [BananaReqDisc] instance.
  BananaReqDisc({

    required  this.length,

    required  this.fruitType,
  });

  @JsonKey(
    
    name: r'length',
    required: true,
    includeIfNull: false
  )


  final int length;



  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is BananaReqDisc &&
     other.length == length &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    length.hashCode +
    fruitType.hashCode;

  factory BananaReqDisc.fromJson(Map<String, dynamic> json) => _$BananaReqDiscFromJson(json);

  Map<String, dynamic> toJson() => _$BananaReqDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

