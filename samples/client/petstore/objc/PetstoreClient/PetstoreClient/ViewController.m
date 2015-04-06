//
//  ViewController.m
//  PetstoreClient
//
//  Created by Tony Tam on 10/18/13.
//  Copyright (c) 2013 Reverb. All rights reserved.
//

#import "ViewController.h"
#import "SWGPetApi.h"

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
    
/*
    SWGPetApi * api = [[SWGPetApi alloc] init];
    [api getPetByIdWithCompletionBlock:@10 completionHandler:^(SWGPet *output, NSError *error) {
        NSLog(@"%@", [output asDictionary]);
        [output set_id:@101];
        [api addPetWithCompletionBlock:output completionHandler:^(NSError *error) {
            NSLog(@"Done!");
        }];

//         load data into file
    }];
    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"test-1" ofType:@"png"];
    NSData *myData = [NSData dataWithContentsOfFile:filePath];
    
    SWGFile *file = [[SWGFile alloc] initWithNameData:@"test-2.png" mimeType:@"image/png" data:myData];
    [api uploadFileWithCompletionBlock:@1
                    additionalMetadata:@"some metadata"
                                  file:file
                     completionHandler:^(NSError *error) {
                        if(error) {
                          NSLog(@"%@", error);
                        }
                     }
//                     completionHandler:^(SWGApiResponse *output, NSError *error) {
//                         if(error) {
//                             NSLog(@"%@", error);
//                         }
//                         else {
//                             NSLog(@"%@", [output asDictionary]);
//                         }
//                     }
     ];
    */
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
