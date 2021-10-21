
## What is the purpose? 

Running `./save_dart_keywords.sh` from this directory will generate `dart-keywords.txt` which contains the current Dart keywords and is used by the dart generator as a set of reserved words. 

## When should the keywords be generated?

`save_dart_keywords.sh` should be run when a new version of Dart is release to update the keywords file.

The last Dart version that was used to generate the keywords can be found in `dart-version.txt`.

## What does the shellscript do? 

1. compile a dart app 
    - `dart2native save-dart-keywords.dart`

1. run the dart app 
    - `./save-dart-keywords.exe`

1. the output is `dart-keywords.txt` and `dart-version.txt`
