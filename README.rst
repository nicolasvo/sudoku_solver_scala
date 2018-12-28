Sudoku
------

Instructions
------------

- Run with IntelliJ
- Run with Docker

.. code-block:: bash

    docker run --rm --name scala -w /home -v $PWD/sudoku:/home frolvlad/alpine-scala scala -I sudoku.scala
