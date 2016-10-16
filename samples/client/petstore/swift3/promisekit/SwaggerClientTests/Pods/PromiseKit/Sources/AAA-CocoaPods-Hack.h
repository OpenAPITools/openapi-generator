// this file because CocoaPods orders headers alphabetically
// in its generated Umbrella header. We need its generated
// header because to ensure subspec headers are part of the
// complete module.
//
// Without this header AnyPromise.h is imported first which then
// imports PromiseKit.h, PromiseKit.h imports AnyPromise.h, which
// in this scenario is ignored because it is already being 
// imported.
//
// A better technical fix would be to move the declarations in
// PromiseKit.h away thus making it a real Umbrella header…

#import <PromiseKit/PromiseKit.h>
