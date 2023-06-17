//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/banana_req_disc.dart';
import 'package:openapi/src/model/apple_req_disc.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_req_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitReqDisc {
  /// Returns a new [FruitReqDisc] instance.
  FruitReqDisc({

    required  this.seeds,

    required  this.fruitType,

    required  this.length,
  });

  @JsonKey(
    
    name: r'seeds',
    required: true,
    includeIfNull: false
  )


  final int seeds;



  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @JsonKey(
    
    name: r'length',
    required: true,
    includeIfNull: false
  )


  final int length;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FruitReqDisc &&
     other.seeds == seeds &&
     other.fruitType == fruitType &&
     other.length == length;

  @override
  int get hashCode =>
    seeds.hashCode +
    fruitType.hashCode +
    length.hashCode;

  factory FruitReqDisc.fromJson(Map<String, dynamic> json) => _$FruitReqDiscFromJson(json);

  Map<String, dynamic> toJson() => _$FruitReqDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

