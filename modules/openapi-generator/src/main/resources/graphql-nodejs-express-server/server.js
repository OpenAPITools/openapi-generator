import express from 'express';
import {ApolloServer, graphiqlExpress, graphqlExpress} from 'apollo-server-express'
import combine from 'graphql-combine'
import path from 'path'

const PORT = 4000 || process.env;

// Initialize the app
const app = express();

// Get combined typeDefs and resolvers object
const {typeDefs, resolvers} = combine({
    // TypeDefs glob pattern
    typeDefs: path.join(__dirname, '**/*.graphql'),

    // Resolvers glob pattern
    resolvers: path.join(__dirname, 'api/*_resolver.js')
});

// GraphQL: Schema
const server = new ApolloServer({
    typeDefs: typeDefs,
    resolvers: resolvers,
    playground: {
        endpoint: `http://localhost:${PORT}/graphql`,
        settings: {
            'editor.theme': 'light'
        }
    }
});

server.applyMiddleware({app: app});

// Start the server
app.listen(PORT, () => {
    console.log(`You can reach GraphQL at: http://localhost:${PORT}/graphql`);
});