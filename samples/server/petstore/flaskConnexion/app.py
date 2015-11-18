#!/usr/bin/env python3

import connexion

if __name__ == '__main__':
    app = connexion.App(__name__, 8080,
    specification_dir='./swagger/')
    app.add_api('swagger.yaml', arguments={'title': 'This is a sample server Petstore server.  You can find out more about Swagger at &lt;a href=\&quot;http://swagger.io\&quot;&gt;http://swagger.io&lt;/a&gt; or on irc.freenode.net, #swagger.  For this sample, you can use the api key \&quot;special-key\&quot; to test the authorization filters'})
    app.run()
