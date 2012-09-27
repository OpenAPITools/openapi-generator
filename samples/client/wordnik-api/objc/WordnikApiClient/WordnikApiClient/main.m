#import <Foundation/Foundation.h>
#import "NIKWordApi.h"

int main(int argc, const char * argv[])
{
    @autoreleasepool {
        static bool done = false;
        
        NIKWordApi * api = [[NIKWordApi alloc] init];
        [api addHeader:@"a2a73e7b926c924fad7001ca3111acd55af2ffabf50eb4ae5" forKey:@"api_key"];

        static NIKWordObject* word = nil;
        static NSError * gError = nil;
        [api getWordWithCompletionBlock:@"cat" useCanonical:@"true" includeSuggestions:@"true" completionHandler:^(NIKWordObject *output, NSError *error) {
            if(error) {
                gError = error;
            }
            if(output == nil){
                NSLog(@"failed to fetch pet");
            }
            else {
                word = [[NIKWordObject alloc] initWithValues:[output asDictionary]];
            }
        }];
        
        NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
        while(!done && [loopUntil timeIntervalSinceNow] > 0){
            if(gError){
                NSLog(@"got error %@", gError);
                done = true;
            }
            if(word){
                NSLog(@"word: %@", [word asDictionary]);
                done = true;
            }
        }
    }

    return 0;
}

void etys() {
    static bool done = false;
    
    NIKWordApi * api = [[NIKWordApi alloc] init];
    [api addHeader:@"a2a73e7b926c924fad7001ca3111acd55af2ffabf50eb4ae5" forKey:@"api_key"];
    
    static NSArray* etys = nil;
    static NSError * gError = nil;
    
    [api getEtymologiesWithCompletionBlock:@"catalog" useCanonical:@"false" completionHandler:^(NSArray *output, NSError *error) {
        if(error) {
            gError = error;
        }
        if(output == nil){
            NSLog(@"failed to fetch pet");
        }
        else {
            etys = [[NSArray alloc] initWithArray:output];
        }
    }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            NSLog(@"got error %@", gError);
            done = true;
        }
        if(etys){
            NSLog(@"etys: %@", etys);
            done = true;
        }
    }
}

