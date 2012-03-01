Haskell bindings for the [Secure Inter-Process Communications Library](http://oss.tresys.com/projects/sipc) for SELinux.

Installation
------------

The SIPC C library must be installed before the bindings. Please download it from [this page](http://oss.tresys.com/projects/sipc/wiki/download) and follow the installation instructions.

Once the library is available, Haskell bindings can be installed using `cabal`:

    cabal update
    cabal install bindings-sipc

Testing
-------

Example programs are provided to test message queues. Before using them you need to copy `data.txt` from the SIPC examples directory, and you need to create a file `sipc_mq_test`. The programs can then be executed in order: `MQCreator` ==> `MQSender` -> `MQReader`, `MQDestroyer`.
