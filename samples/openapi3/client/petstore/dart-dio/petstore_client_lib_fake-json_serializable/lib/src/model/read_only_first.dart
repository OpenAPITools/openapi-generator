//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'read_only_first.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ReadOnlyFirst {
  /// Returns a new [ReadOnlyFirst] instance.
  ReadOnlyFirst({

     this.bar,

     this.baz,
  });

  @JsonKey(
    
    name: r'bar',
    required: false,
    includeIfNull: false
  )


  final String? bar;



  @JsonKey(
    
    name: r'baz',
    required: false,
    includeIfNull: false
  )


  final String? baz;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ReadOnlyFirst &&
     other.bar == bar &&
     other.baz == baz;

  @override
  int get hashCode =>
    bar.hashCode +
    baz.hashCode;

  factory ReadOnlyFirst.fromJson(Map<String, dynamic> json) => _$ReadOnlyFirstFromJson(json);

  Map<String, dynamic> toJson() => _$ReadOnlyFirstToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

