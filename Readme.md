Servant Subscriber
==================

[![Build Status](https://travis-ci.org/eskimor/servant-subscriber.svg?branch=master)](https://travis-ci.org/eskimor/servant-subscriber)

Servant-subscriber enables your clients to subscribe to resources in your servant-api (an API endpoint).
Servant-subscriber will then notify the client via a WebSocket connection whenever the resource
changes, thus the client can easily stay up to date with a resource.

## Status

It seems to work - it is already tested in [examples/central-counter](https://github.com/eskimor/servant-purescript/tree/master/examples/central-counter) of servant-purescript and used in production.

Still missing:

 - Documentation, announcement.
 - Tests 
