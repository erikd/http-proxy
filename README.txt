A Haskell library for creating HTTP and HTTPS web proxies. Memory usage of the
proxy scales with the number of simultaneous connections and should be
independent of the size of either the POST request body or the response body.

This library relies heavily on the following libraries:

   - enumerator : Efficient, predictable and safe alternative to lazy I/O.
   - http-enumerator : HTTP protocol build on top of enumerator.
   - Wai : A common protocol between web servers and clients.

The code for this library was originally hacked out of Michael Snoyman's Warp
web server by Stephen Blackheath. Erik de Castro Lopo then converted it from an
application to a library from which to build web proxies.

This is still pre-alpha quality.

Proxying currently works for many of the standard cases but is also known to
fail for some others. Work is underway to fix these issues.


Known Issues
============

 - Some URLs get mangled by the proxy.


Required Features
=================

 - A config hook to allow the request sent by the client to be modified before
   the proxy acts on it.

