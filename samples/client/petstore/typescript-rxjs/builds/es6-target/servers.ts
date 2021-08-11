/**
 *
 * Represents the configuration of a server including its
 * url template and variable configuration based on the url.
 *
 */
export class ServerConfiguration<T extends { [key: string]: string }> {
    public constructor(private url: string, private variableConfiguration: T, private description: string) {}

    /**
     * Sets the value of the variables of this server.
     *
     * @param variableConfiguration a partial variable configuration for the variables contained in the url
     */
    public setVariables(variableConfiguration: Partial<T>) {
        Object.assign(this.variableConfiguration, variableConfiguration);
    }

    public getConfiguration(): T {
        return this.variableConfiguration
    }

    public getDescription(): string {
        return this.description
    }

    /**
     * Constructions the URL this server using the url with variables
     * replaced with their respective values
     */
    public getUrl(): string {
        let replacedUrl = this.url;
        for (const key in this.variableConfiguration) {
            var re = new RegExp("{" + key + "}","g");
            replacedUrl = replacedUrl.replace(re, this.variableConfiguration[key]);
        }
        return replacedUrl
    }
}

const server1 = new ServerConfiguration<{  }>("http://petstore.swagger.io/v2", {  }, "")

export const servers = [server1];
