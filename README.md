#This is a webapp to manage costs and reciepts.

The main page shows a list of reciepts or single expendures. Upon clicking on a reciept, it will expand to show what it consists of.

To see a working demo, go to http://desolate-anchorage-3504.herokuapp.com/ and log in as 'bla', password 'bla'.

# Setup
* install quicklisp: https://www.quicklisp.org/beta/
* install postgres: https://www.postgresql.org/
* add a new database that conforms with `*local-db-params*`, e.g.:
```
CREATE USER dan WITH PASSWORD 'password';
CREATE DATABASE costs;
GRANT ALL PRIVILEGES ON DATABASE costs to dan;
```
* start the application: `sbcl --load reciept.lisp`


# Users

user passwords are simply (sha256 (concatenate 'string salt password))
