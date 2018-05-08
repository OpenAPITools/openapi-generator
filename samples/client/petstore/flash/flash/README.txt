README for the Flash application. 

These instructions are given using the version 4.7 of Flash Builder. 

How to use the Flash client library:

The code which is given to you is to be considered as a Library. Therefore, in Flash Builder, you have to:
1. Create an ActionScript Library Project. (Warning: If you are dealing with Files as parameter, you need to include the Adobe AIR libraries)
2. Copy the src folder from the flash library to the project.
3. Copy the lib folder.
4. Copy the build.properties and build.xml. (You can update the paths in build.properties)
5. Right+Click on the project folder and go to Properties. Then go to the tab 'ActionScript Library Build Path' and add these two SWC: 'as3corelib.swc' and 'ASAXB-0.1.1.swc'. (They are located in the folder lib)
6. Create the Flash Application

How to use the Flash client library in an application:

Using the Flash client library (especially getting the response message from the server) is a bit tricky.

The response message is given through an EventDispatcher. Therefore, you have to create an EventDispatcher and listen to the endpoints that you're calling. Below, you will find a pseudo-code explaining how to do this.

After creating an application in java, you will have a mxml file. In this mxml file, you need to declare a Script (given below) after the Declarations.

<fx:Script>
<![CDATA[

    // Import the different libraries you need
    // ... 

    // Import the org.openapitools libraries
    import org.openapitools.common.ApiUserCredentials;
	import org.openapitools.event.ApiClientEvent;
    import org.openapitools.client.api.[[API]]; // Put your Api here.

    // Variables
	private var cred:ApiUserCredentials;
	private var dispatcher:EventDispatcher;
    private var myAPI:[[API]]; // Change [[API]] to be the real name

    // Function that print in the console the message of the response
    public function onEvent(event:ApiClientEvent):void {
        // trace only prints when in debug mod. Be carefule about that.		
        trace(event.response.payload); 
	}   

    // Main function
    public main():void {
        // Define a dispatcher
		dispatcher = new EventDispatcher();
        // Define the EventListener. Fill the [[function]] by the name of the function you have in
        // the API file.  
		dispatcher.addEventListener([[function]], onEvent);

        // To create the API, you need the ApiUserCredentials and the dispatcher.
        cred = new ApiUserCredentials([[host]], [[basePath]] , [[apiToken]]); 
        // You can add more parameters. Go see the function in ApiUserCredential.as in org/openapitools/common

        // Create the API
        myAPI = new [[API]](cred, dispatcher); // change [[API]] to be the real name
        
        // Now, you can use the API
        myAPI.[[function]]([[PARAMETERS]]);
        // Change [[function]] and add your parameters. Then, you will see the response in the console.
    }     
]]>
</fx:Script>
