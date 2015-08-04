# http-proxy

[![Build Status](https://secure.travis-ci.org/erikd/http-proxy.png?branch=master)](http://travis-ci.org/erikd/http-proxy)

A Haskell library for creating HTTP and HTTPS web proxies.

The aim is to make all proxying operations work in constant space (per
connection) so that memory usage scales linearly with the number of concurrent
connections and is completely independent of the size of either the POST
request body or the response body.

This library relies heavily on the following libraries:

* wai : A common protocol between web servers and clients.
* warp : The web servers the proxy application runs in.
* http-conduit / http-client : Perform the upstream requests.

This is still beta quality.
