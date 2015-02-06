//
//  ViewController.m
//  swaggerClient
//
//  Created by Tony Tam on 2/5/15.
//  Copyright (c) 2015 Eatbacon. All rights reserved.
//

#import "ViewController.h"
#import "SWGPetApi.h"

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view, typically from a nib.

    SWGPetApi * pet = [[SWGPetApi alloc] init];
    [pet getPetByIdWithCompletionBlock:@10 completionHandler:^(SWGPet *output, NSError *error) {
        NSLog(@"%@", output);
    }];
    
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
