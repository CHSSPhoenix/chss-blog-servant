# chss-blog-servant

## How to build and run

You will need to install [SQLite](https://sqlite.org), and then type this in the project:

```
stack clean
stack build
stack exec chss-blog-servant-exe
```

## How to test
- Make a request to `http://localhost:8080/users`
