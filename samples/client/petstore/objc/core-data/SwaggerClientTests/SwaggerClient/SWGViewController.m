//
//  SWGViewController.m
//  SwaggerClient
//
//  Created by geekerzp on 06/26/2015.
//  Copyright (c) 2014 geekerzp. All rights reserved.
//

#import "SWGViewController.h"
#import <SwaggerClient/SWGApiClient.h>
#import <SwaggerClient/SWGPet.h>
#import <SwaggerClient/SWGPetApi.h>
#import <SwaggerClient/SWGDefaultConfiguration.h>

@interface SWGViewController ()

@end

@implementation SWGViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    SWGDefaultConfiguration *config = [SWGDefaultConfiguration sharedConfig];
    config.debug = YES;
    
    SWGPetApi *api = [[SWGPetApi alloc] init];
    NSURL *file = [NSURL fileURLWithPath:@"/Users/geekerzp/tmp/test.jpg"];
    [api uploadFileWithPetId:@2 additionalMetadata:@"2" file:file completionHandler:^(NSError *error) {
        NSLog(@"*** error: %@", error);
    }];
}

- (SWGPet*) createPet {
    SWGPet * pet = [[SWGPet alloc] init];
    pet._id = @((long) [[NSDate date] timeIntervalSince1970]);
    pet.name = @"monkey";
    
    SWGCategory * category = [[SWGCategory alloc] init];
    category._id = [[NSNumber alloc] initWithInteger:arc4random_uniform(100000)];
    category.name = @"super-happy";
    pet.category = category;
    
    SWGTag *tag1 = [[SWGTag alloc] init];
    tag1._id = [[NSNumber alloc] initWithInteger:arc4random_uniform(100000)];
    tag1.name = @"test tag 1";
    SWGTag *tag2 = [[SWGTag alloc] init];
    tag2._id = [[NSNumber alloc] initWithInteger:arc4random_uniform(100000)];
    tag2.name = @"test tag 2";
    pet.tags = (NSArray<SWGTag> *) @[tag1, tag2];

    pet.status = @"available";

    NSArray * photos = @[@"http://foo.bar.com/3", @"http://foo.bar.com/4"];
    pet.photoUrls = photos;
    return pet;
}

@end
