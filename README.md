#Current Features:
* Computes closure of any { table, mvds, fds } to create { table, mvds+, fds+ }
* Produces all minimal decompositions of a given relation into 4nf relations.

Currently quite rough, performance is around n^3n! relative to the number of columns in a database if the given fds and mvds scale with it.

Edit main.rs to change the database schema and given fds and mvds. First few lines should give an example of how it works.
