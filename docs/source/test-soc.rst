########
Test SoC
########

The C-class repository also contains a simple test-soc for the purpose of simulating applications
and verifying the core. More enhanced and open-source SoCs can be found `here <https://gitlab.com/shaktiproject/cores/shakti-soc>`_.

Structure of SoC
----------------
The Test-SoC has the following structure (defined to a max of 4 levels of depth):

.. mermaid::

   graph TD;
      X[mkTbSoC] --> A(mkSoC)
      X --> B(mkbram)
      X --> C(mkbootrom)
      A --> D(mkccore_axi4)
      A --> E(mkuart)
      A --> F(mkclint)
      A --> G(mksignature_dump)
      D --> H(mkriscv)
      D --> I(mkdmem)
      D --> J(mkimem)

Description of the above modules:

  +--------------------+----------------------------------------------------------+
  | Module-Name        | Description                                              |
  +--------------------+----------------------------------------------------------+
  | mkriscv            | Contains the 5-stages of the core pipeline including the | 
  |                    | execution and only the interface to the memory subsystem |
  +--------------------+----------------------------------------------------------+
  | mkdmem             | The Data memory subsystem. Includes the data-cache and   |
  |                    | data-tlbs                                                |
  +--------------------+----------------------------------------------------------+
  | mkimem             | The instruction memory subsystem. Includes the           |
  |                    | instruction-cache and the instruction-tlbs               |
  +--------------------+----------------------------------------------------------+
  | mkccore_axi4       | Contains the above modules and the integrations across   |
  |                    | them. Also provides 3 AXI-4 interfaces to be connected to| 
  |                    | the Cross-bar fabric                                     |
  +--------------------+----------------------------------------------------------+
  | mkuart             | UART module                                              |
  +--------------------+----------------------------------------------------------+
  | mkclint            | Core Level Interrupt                                     |
  +--------------------+----------------------------------------------------------+
  | mksignature_dump   | Signature dump module (for simulation only)              |
  +--------------------+----------------------------------------------------------+
  | mkSoc              | contains all the above modules and instantiates the AXI-4| 
  |                    | crossbar fabric as well. The fabric has 2 additional     |
  |                    | slaves, which are brought out through the interface to   |
  |                    | connect to the boot-rom and bram-main-memory present in  |
  |                    | the Test-bench                                           |
  +--------------------+----------------------------------------------------------+
  | mkbram             | BRAM based memory acting as main-memory                  |
  +--------------------+----------------------------------------------------------+
  | mkbootrom          | Bootrom slave                                            |
  +--------------------+----------------------------------------------------------+
  | mkTbSoC            | Testbench that instantiates the Soc, and integrates it   |
  |                    | with the bootrom and a bram memory                       |
  +--------------------+----------------------------------------------------------+

The details of the devices can be found in `devices <https://gitlab.com/shaktiproject/uncore/devices/>`_

Address Map of SoC
------------------

  +----------------+-------------------------+
  | Module         | Address Range           |
  +----------------+-------------------------+
  | BRAM-Memory    | 0x80000000 - 0x8FFFFFFF |
  +----------------+-------------------------+
  | BootROM        | 0x00001000 - 0x00010FFF |
  +----------------+-------------------------+
  | UART           | 0x00011300 - 0x00011340 |
  +----------------+-------------------------+
  | CLINT          | 0x02000000 - 0x020BFFFF |
  +----------------+-------------------------+
  | Debug-Halt Loop| 0x00000000 - 0x0000000F |
  +----------------+-------------------------+
  | Signature Dump | 0x00002000 - 0x0000200c |
  +----------------+-------------------------+

Please note that the bram-based memory in the test-bench can only hold upto 256MB of code.
Thus the elf2hex arguments will need to applied accordingly

BootRom Content
---------------

By default, on system-reset the core will always jump to ``0x1000`` which is mapped to the bootrom. 
The bootrom is initialized using the files ``boot.MSB`` and ``boot.LSB``. The bootrom immediately 
causes a re-direction to address ``0x80000000`` where the main program is expected to lie. 
It is thus required that all programs are linked with text-section begining at ``0x80000000``. 
The rest of the boot-rom holds a dummy device-tree-string information.

Synthesis of Core
-----------------

When synthesizing for an FPGA/ASIC, the top module should be ``mkccore_axi4 (mkccore_axi4.v)`` 
as the top module. 

The ``mkimem`` and ``mkdmem`` module include SRAM instances which implement the respective data 
and tag arrays. These are implemented as BRAMs and thus require no changes for FPGAs. 
However for an ASIC flow, it is strictly advised to replace the BRAMs with respective SRAMs. 
The user should refer to :ref:`RAM Structures<ram-structures-label>` for correctly performing the replacement.
