#if TARGET_OS_MAC && !TARGET_OS_EMBEDDED && !TARGET_OS_SIMULATOR

#import <Foundation/NSTask.h>
#import <PromiseKit/AnyPromise.h>

#define PMKTaskErrorLaunchPathKey @"PMKTaskErrorLaunchPathKey"
#define PMKTaskErrorArgumentsKey @"PMKTaskErrorArgumentsKey"
#define PMKTaskErrorStandardOutputKey @"PMKTaskErrorStandardOutputKey"
#define PMKTaskErrorStandardErrorKey @"PMKTaskErrorStandardErrorKey"
#define PMKTaskErrorExitStatusKey @"PMKTaskErrorExitStatusKey"

/**
 To import the `NSTask` category:

     use_frameworks!
     pod "PromiseKit/Foundation"

 Or `NSTask` is one of the categories imported by the umbrella pod:

     use_frameworks!
     pod "PromiseKit"
 
 And then in your sources:

    #import <PromiseKit/PromiseKit.h>
*/
@interface NSTask (PromiseKit)

/**
 Launches the receiver and resolves when it exits.

 If the task fails the promise is rejected with code `PMKTaskError`, and
 `userInfo` keys: `PMKTaskErrorStandardOutputKey`,
 `PMKTaskErrorStandardErrorKey` and `PMKTaskErrorExitStatusKey`.
 
     NSTask *task = [NSTask new];
     task.launchPath = @"/usr/bin/basename";
     task.arguments = @[@"/usr/bin/sleep"];
     [task promise].then(^(NSString *stdout){
         //…
     });

 @return A promise that fulfills with three parameters:

  1) The stdout interpreted as a UTF8 string.
  2) The stderr interpreted as a UTF8 string.
  3) The stdout as `NSData`.
*/
- (AnyPromise *)promise NS_REFINED_FOR_SWIFT;

@end

#endif
