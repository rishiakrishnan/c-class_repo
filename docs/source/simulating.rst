###################
Simulating the Core
###################

Generate Verilated Executable
-----------------------------

.. code-block:: bash

  $ cd c-class
  $ python -m configure.main -ispec sample_config/default.yaml
  $ make

The above should result in following files in the ``bin`` folder:

 - out
 - boot.LSB
 - boot.MSB

Executing Programs
------------------

Let's assume the software program that you would like to simulate on the core is called 
``prog.elf`` (compiled using standard riscv-gcc). This elf needs to be converted
to a hex file which can be provided to the verilated executable: ``out``. This
hex can be generated using the following command:

For 64-bit:

.. code-block:: bash

  $ elf2hex 8 33554432 bbl 2147483648 > code.mem

For 32-bit:

.. code-block:: bash

  $ elf2hex 4 67108864 add.elf 2147483648 > code.mem

place the ``code.mem`` file in the ``bin`` folder and execute the ``out`` binary
to initiate simulation.

Please note, since the boot code in the bootrom implicitly jumps to ``0x80000000`` the programs 
should also be compiled at ``0x80000000``. Plus the bram main memory is 256MB large. 

Support for PutChar
-------------------

The test-soc for simulation contains a simple uart. The ``putchar`` function for the same is available 
`HERE <https://gitlab.com/shaktiproject/uncore/devices/blob/master/uart/uart_driver.c>`_. 
This has to be used in the printf functions. The output of the ``putchar`` is captured in a separate 
file app_log during simulation.

Simulation Arguments (Logger Utility)
-------------------------------------

1. ``./out +rtldump``: if the core has been configured with ``trace_dump: true``
   , then a rtl.dump
   file is created which shows the trace of instruction execution. Each line
   in the file has the following format:

   <privilege-mode> <program-counter> <instruction> <register-updated><register value>

2. To enable printing of debug statements from the bluespec code, one can pass
   custom logger arguments to the simulation binary as follows

   - ``./out +fullverbose``: prints all the logger statements across all modules
     and all levels of verbosity
   - ``./out +mstage1 +l0``: prints all the logger statements within module
     stage1 which are at verbosity level 0. 
   - ``./out +mstage2 +mstage4 +l0 +l3``: prints all the logger statements
     within modules stage2 and stage4 which are at verbosity levels 0 and 3
     only.
      
3. An ``app_log`` file is also created which captures the output of the uart,
   typically used in the ``putchar`` function in C/C++ codes as mentioned
   above.

Connect to GDB in Simulation
----------------------------

A debugger implementation following the riscv-debug-draft-014 has been integrated with the core.
This can be instantiated in the design by configuring with: ``debugger_support: true``

Perform the following steps to connect to the core executable with a gdb terminal. 
This assumes you have installed openocd and is available as part of you `$PATH` variable.

Modify the ``sample_config/default.yaml`` to enable:  debugger_support and open_ocd. 
Generate a new executable with this config to support jtag remote-bitbang in the
test-bench

.. code-block:: bash

  $ python -m configure.main -ispec sample_config/default.yaml
  $ make gdb # generate executable with open-ocd vpi enabled in the test-bench

1. Simulate the RTL
   In a new terminal do the following:
   
   .. code-block:: bash
   
     $ cd c-class/bin/
     $ ./out > /dev/null

2. Connect to OpenOCD
   Open a new terminal and type the following:
   
   .. code-block:: bash
   
   
     $ cd c-class/test_soc/gdb_setup/
     $ openocd -f shakti_ocd.cfg

3. Connect to GDB
   Open yet another terminal and type the following:
   
   .. code-block:: bash
   
     $ cd c-class/test_soc/gdb_setup
     $ riscv64-unknown-elf-gdb -x gdb.script

In this window you can now perform gdb commands like : ``set $pc, i r, etc``

To reset the SoC via the debugger you can execute the following within the gdb shell:

.. code:: bash

  $ monitor reset halt
  $ monitor gdb_sync
  $ stepi
  $ i r

.. note:: The above will not reset memories like caches, brams, etc

Dhrystone
---------------------

The max DMIPS of the core is **1.72DMIPs/MHz.**

.. code:: bash

  $ git clone https://gitlab.com/shaktiproject/cores/benchmarks.git
  $ cd benchmakrs
  $ make dhrystone ITERATIONS=100000

the ``output`` directory will contain a code.mem file which needs to be copied
to the ``bin`` and execute the cclass verilated binary:

.. code:: bash

   $ cp benchmarks/output/code.mem c-class/bin # change paths accordingly
   $ cd c-class/bin
   $ ./out
   $ cat app_log

      Microseconds for one run through Dhrystone:     10.0
      Dhrystones per Second:                       95746.0


Linux on C-Class
----------------

1. Generate RTL using the default.yaml config as provided in the repo

   .. code-block:: bash

    $ python -m configure.main -ispec sample_config/default.yaml
    $ make # generate executable

2. Download the shakti-linux repository  and generate the kernel image:

   .. code-block:: bash

     $ git clone https://gitlab.com/shaktiproject/software/shakti-linux
     $ cd shakti-linux
     $ export SHAKTI_LINUX=$(pwd)
     $ git submodule update --init --recursive
     $ cd $SHAKTI_LINUX
     $ make -j16 ISA=rv64imafd

3. Come back to the folder c-class/ to simulate the kernel on the
   C-class executable:

   .. code-block:: bash

     $ cd c-class/
     $ cp $SHAKTI_LINUX/work/riscv-pk/bbl ./bin/
     $ cd bin
     $ elf2hex 8 33554432 bbl 2147483648 > code.mem
     $ ./out

   Track the ``app_log`` file to see the kernel messages being printed

FreeRTOS on C-class
-------------------

1. Generate a 32-bit RTL with the following command:
   
   .. code-block:: bash

    $ python -m configure.main -ispec sample_config/freertos.yaml
    $ make # generate executable

2. Download the free-RTOS repository for C-class
   
   .. code-block:: bash

    $ git clone https://gitlab.com/shaktiproject/software/FreeRTOS
    $ cd FreeRTOS/FreeRTOS-RISCV/Demo/shakti/
    $ make

3. Come back to the c-class folder and do the following:

   .. code-block:: bash

     $ cd c-class/
     $ cp FreeRTOS/FreeRTOS-RISCV/Demo/shakti/frtos-shakti.elf ./bin
     $ cd bin
     $ elf2hex 8 4194304 frtos-shakti.elf 2147483648 > code.mem
     $ ./out
   
   Track the ``app_log`` file to see the kernel messages being printed
