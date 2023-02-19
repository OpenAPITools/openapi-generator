//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'model_file.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ModelFile {
  /// Returns a new [ModelFile] instance.
  ModelFile({

     this.sourceURI,
  });

      /// Test capitalization
  @JsonKey(
    
    name: r'sourceURI',
    required: false,
    includeIfNull: false
  )


  final String? sourceURI;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelFile &&
     other.sourceURI == sourceURI;

  @override
  int get hashCode =>
    sourceURI.hashCode;

  factory ModelFile.fromJson(Map<String, dynamic> json) => _$ModelFileFromJson(json);

  Map<String, dynamic> toJson() => _$ModelFileToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

