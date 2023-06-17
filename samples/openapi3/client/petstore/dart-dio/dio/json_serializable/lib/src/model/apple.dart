//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'apple.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Apple {
  /// Returns a new [Apple] instance.
  Apple({

     this.kind,
  });

  @JsonKey(
    
    name: r'kind',
    required: false,
    includeIfNull: false
  )


  final String? kind;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Apple &&
     other.kind == kind;

  @override
  int get hashCode =>
    kind.hashCode;

  factory Apple.fromJson(Map<String, dynamic> json) => _$AppleFromJson(json);

  Map<String, dynamic> toJson() => _$AppleToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

