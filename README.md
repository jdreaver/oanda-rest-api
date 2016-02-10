# oanda-rest-api

[![Build Status](https://travis-ci.org/jdreaver/oanda-rest-api.svg)](https://travis-ci.org/jdreaver/oanda-rest-api)

This library implements Haskell client to the
[OANDA REST API](http://developer.oanda.com/rest-live/introduction/).

## Status

There are still parts of the API that are not yet implemented. Here is the
status of each part of the API:

* Rates: Mostly done, just need to refactor so the parameters are cleaner.
* Accounts: Done.
* Orders: Can only get a list of open orders. Can't yet create or modify
  orders.
* Trades: Almost done, just need to add closing a trade.
* Positions: Done.
* Transaction History: Almost done. Could probably refactor the types to be
  more specific, instead of putting `Maybe` everywhere.
