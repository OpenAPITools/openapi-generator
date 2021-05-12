# mothership-seed.sql

## What is this file?

This file is used by the `cc-postgres` repo:
https://github.com/confluentinc/cc-postgres

This file includes the SQL statements used to seed an empty database.
E.g. a postgres container in a CPD environment.

## Testing changes

You can test changes locally:

- In one terminal

    ```bash
    docker-compose up

    ###
    ### Pay attention to errors in the output
    ###
    ```

- In another terminal

    ```bash
    ### params should match docker-compose file
    
    psql -h localhost -p 5432 --username postgres mothership
    ```

You can also test according to steps in `cc-postgres` repo if you want to test
the k8s job as well.