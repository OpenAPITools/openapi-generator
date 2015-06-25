//
//  ViewController.m
//  PetstoreClient
//
//  Created by Tony Tam on 10/18/13.
//  Copyright (c) 2015 SmartBear Software. All rights reserved.
//

#import "ViewController.h"
#import "SWGPet.h"
#import "SWGCategory.h"
#import "SWGTag.h"
#import "SWGPetApi.h"
#import "SWGStoreApi.h"
#import "SWGUserApi.h"
#import "SWGConfiguration.h"
#import "SWGMythingApi.h"

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    /*
    NSDictionary *cateHash = @{ @"id": @123, @"name": @"test name" };
    NSDictionary *tagHash = @{ @"id": @123, @"name": @"test name" };
    NSDictionary *petHash = @{ @"id": @123, @"test": @(YES), @"name": @"test name", @"category": cateHash, @"tags": @[tagHash], @"photoUrls": @[@"test url"] };
    SWGPet *pet = [[SWGPet alloc] initWithDictionary:petHash
                                               error:nil];
    
    SWGPetApi *api = [[SWGPetApi alloc] init];
    [api addPetWithCompletionBlock:pet completionHandler:^(NSError *error) {
        NSLog(@"%@", error);
    }];
    */
    
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
