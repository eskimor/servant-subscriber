Servant Subscriber
==================

[![Build Status](https://travis-ci.org/eskimor/servant-subscriber.svg?branch=master)](https://travis-ci.org/eskimor/servant-subscriber)

Servant-subscriber enables you clients to subscribe to resources in your servant-api (an API endpoint).
Servant-subscriber will then notify the client via a WebSocket connection whenever the resource
changes, thus the client can easily stay up to date all the time.

## Status

It seems to work - it is already tested in examples/central-counter of servant-purescript.

Still missing:

 - Client code generation with servant-purescript for subscriptions.
 - Client side code (purescript-subscriber) currently only contains a very low-level API which
   is also about to change.
 - Documentation, blog post.
 - Tests

I will now continue with my actual project which uses servant-subscriber and add the missing parts when
they come along.
