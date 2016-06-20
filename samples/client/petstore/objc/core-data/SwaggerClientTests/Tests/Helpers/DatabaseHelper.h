#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

@interface DatabaseHelper : NSObject

+ (NSManagedObjectContext *)createContextWithModelName:(NSString *)mName;

+ (void)clearContext:(NSManagedObjectContext *)ctx fromEntitiesWithName:(NSString *)entityName;

+ (NSManagedObjectModel *)createModelWithModelName:(NSString *)mName;

+ (NSManagedObjectContext *)createDatabaseWithModel:(NSManagedObjectModel*)model;

@end
