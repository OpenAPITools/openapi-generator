#import "DatabaseHelper.h"

@implementation DatabaseHelper


+ (NSManagedObjectContext *)createContextWithModelName:(NSString *)mName {
    NSManagedObjectModel *model = [self createModelWithModelName:mName];
    return [self createDatabaseWithModel:model];
}

+ (NSManagedObjectContext *)createDatabaseWithModel:(NSManagedObjectModel*)model {
    NSPersistentStoreCoordinator *coordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:model];
    [coordinator addPersistentStoreWithType:NSInMemoryStoreType configuration:nil URL:nil options:nil error:nil];

    NSManagedObjectContext *testingContext = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];
    [testingContext setPersistentStoreCoordinator:coordinator];
    [testingContext setMergePolicy:NSMergeByPropertyObjectTrumpMergePolicy];
    return testingContext;
}

+ (NSManagedObjectModel *)createModelWithModelName:(NSString *)mName {
    NSBundle *bundle = [NSBundle bundleForClass:[self class]];
    NSString *path = [bundle pathForResource:mName ofType:@"momd"];
    NSAssert(path, @"Missing Model for name: %@",mName);
    NSURL *modURL = [NSURL fileURLWithPath:path];
    return [[NSManagedObjectModel alloc] initWithContentsOfURL:modURL];
}

+ (void)clearContext:(NSManagedObjectContext *)ctx fromEntitiesWithName:(NSString *)entityName {
    NSFetchRequest *fetch = [[NSFetchRequest alloc] init];
    [fetch setEntity:[NSEntityDescription entityForName:entityName inManagedObjectContext:ctx]];
    NSError *error = nil;
    NSArray *result = [ctx executeFetchRequest:fetch error:&error];
    if (error) {
        NSLog(@"Failed clearing context from entities with name [%@]", error);
    }
    for (id basket in result) {
        [ctx deleteObject:basket];
    }
}

@end
