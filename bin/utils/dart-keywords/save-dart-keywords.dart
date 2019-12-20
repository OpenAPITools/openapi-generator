import 'dart:io';

import 'package:analyzer/dart/ast/token.dart';

main() {
  // save keywords to a text file
  final txtFile = File(
      '../../../modules/openapi-generator/src/main/resources/dart/dart-keywords.txt');
  String txtString = '';
  for (String keyword in Keyword.keywords.keys.toList()) {
    txtString += keyword + '\n';
  }
  txtString =
      txtString.substring(0, txtString.length - 1); // remove last newline
  txtFile.writeAsStringSync(txtString);
}
