fibas
=====

Fibonacci as a service

Build
-----

To build, test and run the service:

    $ make all

HTTP API Usage examples
-----

Number API:
----
`GET /fib/:index`

Usage example:
```curl
curl -X GET "http://localhost:8080/fib/58"
```

Response:
```javascript
{"index":58,"fibonacci":591286729879}
```


List API: 
----
`GET /fib/:end_index/list[?page_size=:page_size][&start_index=:start_index]`

Usage example:
```curl
curl -X GET "http://localhost:8080/fib/58/list?page_size=5&start_index=13"
```

Response:
```javascript
{"next_start_index":18,
 "list":[{"index":13,"fibonacci":233},
         {"index":14,"fibonacci":377},
         {"index":15,"fibonacci":610},
         {"index":16,"fibonacci":987},
         {"index":17,"fibonacci":1597}]}
```

Blacklist API: 
----
`PUT/DELETE /fib/:index/blacklist`

Usage example:
```curl
curl -X PUT "http://localhost:8080/fib/58/blacklist"
curl -X GET "http://localhost:8080/fib/58"
curl -X GET "http://localhost:8080/fib/59/list?page_size=3&start_index=57"
curl -X DELETE "http://localhost:8080/fib/58/blacklist"
curl -X GET "http://localhost:8080/fib/58"
```

Response:
```javascript
{"index":58,"fibonacci":"blacklisted"}

{"list":[{"index":57,"fibonacci":365435296162},
         {"index":59,"fibonacci":956722026041}]}

{"index":58,"fibonacci":591286729879}
```