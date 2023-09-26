# Specification: One change address mode

This document specifies how one change address mode is enabled in api. The mode is available in shelley and shared style.
When enabled the transactions choose the first change address in a address pool. This is in contrast to situation where
the mode is off where each transaction choosed the first unused change address. Due to that choice change address pool get inflated.
In one change address mode the same address is used and the address pool is non-icreasing.

## One change address mode in api

The "Wallets > Post" HTTP endpoint allows the creation of the wallet that is in one change address mode.

Specifically:

1. Creation of a shelley/shared wallet

    When `one_change_address_mode` field is set to true the created wallet will reuse the first change address when
    transactions are made. When the wallet is restored all used change addresses will be discovered but new transactions will
    reuse the first change address.

    Example `POST` data for the endpoint:

    ```
    {
    ...
      "one_change_address_mode": true,
    ...
    }
    ```

2. Updating metadata of wallet

    The existent wallet can switch between one change address mode. Turning the mode off/on results in
    bloating/freezing, respectively, change address pool when new transactions are made

    Example `PUT` data for the endpoint to turn off one change address mode:

    ```
    {
      "one_change_address_mode": false
    }
    ```
