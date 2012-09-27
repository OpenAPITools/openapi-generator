#import <Foundation/Foundation.h>
#import "NIKWordApi.h"

int main(int argc, const char * argv[]) {
    static bool done = false;
    @autoreleasepool {
        NIKWordApi * api = [[NIKWordApi alloc] init];
        [api addHeader:@"YOUR_API_KEY" forKey:@"api_key"];
        [api getDefinitionsWithCompletionBlock:@"Hello World"
                                  partOfSpeech:nil
                            sourceDictionaries:nil
                                         limit:nil
                                includeRelated:nil
                                  useCanonical:nil
                                   includeTags:nil
                             completionHandler:^(NSArray * definitions, NSError * error) {
            if(error){
                done = true;
                NSLog(@"failed to get results, did you forget to update the api_key?");
            }
            else if(!definitions) {
                done = true;
                NSLog(@"no results!  Did you put in an invalid word?");
            }
            else {
                for(NIKDefinition * def in definitions ){
                    NSLog(@"%@", [def asDictionary]);
                }
                done = true;
            }
        }];
        
        while(!done){}
    }
    return 0;
}
