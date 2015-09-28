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
#import <SwaggerClient/SWGConfiguration.h>

@interface SWGViewController ()

@end

@implementation SWGViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    SWGConfiguration *config = [SWGConfiguration sharedConfig];
    config.debug = YES;
    
    SWGPetApi *api = [[SWGPetApi alloc] init];
    NSURL *file = [NSURL fileURLWithPath:@"/Users/geekerzp/tmp/test.jpg"];
    [api uploadFileWithCompletionBlock:@2 additionalMetadata:@2 file:file completionHandler:^(NSError *error) {
        NSLog(@"*** error: %@", error);
    }];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
}

- (SWGPet*) createPet {
    SWGPet * pet = [[SWGPet alloc] init];
    pet._id = [[NSNumber alloc] initWithLong:[[NSDate date] timeIntervalSince1970]];
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
    pet.tags = (NSArray<SWGTag> *)[[NSArray alloc] initWithObjects:tag1, tag2, nil];

    pet.status = @"available";

    NSArray * photos = [[NSArray alloc] initWithObjects:@"http://foo.bar.com/3", @"http://foo.bar.com/4", nil];
    pet.photoUrls = photos;
    return pet;
}

@end
