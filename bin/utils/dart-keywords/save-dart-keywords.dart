import 'dart:io';

import 'package:analyzer/dart/ast/token.dart';

main() {
  // save keywords to a text file
  final output = File(
      '../../../modules/openapi-generator/src/main/resources/dart/dart-keywords.txt');
  final keywords = Keyword.keywords.keys.join('\n');
  output.writeAsStringSync(keywords);
}
