//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'banana.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Banana {
  /// Returns a new [Banana] instance.
  Banana({

     this.count,
  });

  @JsonKey(
    
    name: r'count',
    required: false,
    includeIfNull: false
  )


  final num? count;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Banana &&
     other.count == count;

  @override
  int get hashCode =>
    count.hashCode;

  factory Banana.fromJson(Map<String, dynamic> json) => _$BananaFromJson(json);

  Map<String, dynamic> toJson() => _$BananaToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

