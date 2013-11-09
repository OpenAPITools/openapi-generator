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
    
    SWGPetApi * api = [[SWGPetApi alloc] init];
    
    [api getPetByIdWithCompletionBlock:@10 completionHandler:^(SWGPet *output, NSError *error) {
        NSLog(@"%@", [output asDictionary]);
    }];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
