.. _configure-core-label:

##################
Configure the Core
##################

The C-class core is highly parameterized and configurable. By changing a single
configuration the user can generate a core instance randing in size from
embedded micro-controllers to Linux capable high-performance cores.

ISA Level Configurations
------------------------

In RISC-V both, the Unprivileged and the Privileged specs both offer a great amount of choices to
configure an implementation with. The Unprivileged spec offers various extensions and sub-extensions
like Multiply-divide, Floating Point, Atomic, Compressed, etc which a user can choose to implement
or not. 

The Unprivileged Spec on the other hand provides a much more larger space of
configurability to the user. Apart from choosing which privilege modes to implement (Machine,
Hypervisor, Supervisor or User), the spec also provides a huge number of Control and Status
Registers (CSRs) which impact various aspects of the RISC-V system. For example the MISA csr can
be used to dynamically enable or disable execution of certain sub-extensions. Similarly, the valid
and legal values of the satp.mode fields indicate what paging schemes are supported by the
underlying implementation.

To capture all such possible choices of the RISC-V ISA in a single standard format, InCore has
proposed the `RISCV-CONFIG <https://github.com/riscv-software-src/riscv-config>`_ YAML format, 
which has also been adopted by the riscv-community, primarily for the ISA compatibility framework.
The core generator uses the same YAML inputs to control various ISA level features of the core. 
  
Generating CSRs
^^^^^^^^^^^^^^^

For implementing the CSR module, C-Class uses the `CSR-BOX <https://gitlab.com/incoresemi/ex-box/csrbox>`_ 
utility to automatically create a bsv module which implements all the necessary CSRs as per the input YAML specification
provided in riscv-config format. An example of the isa YAML is provided in the sample_config directory.
. CSR-BOX ensures the warl functions specified in the YAML are faithfullty replicated in bsv. Along
with CSRs CSR-BOX also provides methods and logic to handle traps and `xRET` instructions based on
the privileged modes (U, S, H) defined in the `ISA` node of the input yaml. 

Note that the CSR-BOX allows one to split the CSRs into a daisy-chain like fashion to reduce the
impact on timing when instantiating large number of CSRs. Thus,  apart from the isa yaml,
CSR-BOX also requires a `grouping yaml
<https://csrbox.readthedocs.io/en/latest/grp.html#group-yaml-dependencies>`_ file which indicates
which daisy-chain unit should contain which set of CSRs. 

CSR-BOX also takes in an optional `debug spec
<https://riscv-config.readthedocs.io/en/latest/yaml-specs.html#debug-yaml-spec>`_ yaml 
(as defined by riscv-config) to capture basic debug related information like where the parking loop
code of the debug is placed in the memory map. Providing the debug spec, also indicates CSR-BOX to
implement the necessary logic for 
handling custom debug interrupts like halt, resume and step. The Debug csrs must be defined in the
debug spec. TODO provide example LINK

CSR-BOX also allows the user to define custom CSRs that may be required by the the implementation.
C-Class uses a custom csr to control the enabling/disabling of caches and branch predictors. The
details of this CSR are provided :ref:`here <custom_csrs>`. An example YAML containing the 
definition of these CSRs which can be fed into CSR-BOX is available in the sample_config directory.

Other Derived Configuration Settings
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Other than the CSRs, C-Class derives the following parameters from the input isa yaml

  - The `ISA` string indicates what extensions be enabled in Hardware and its associated collaterals
  - The max value in the `supported_xlen` node indicates the `xlen` variable in C-Class. This is
    used to defined the width of the integer register file, alu operations, bypass width, virtual
    address size, etc.
  - The `flen` variable in C-Class is set based on the presence of 'F' or 'D' characters in the ISA
    string.
  - If the 'S' extension is present in the `ISA` string, then C-Class detects the supervisor page
    translation mode to be implemented by detecting the max legal values of the `satp.mode` csr field
    present in the input yaml
  - The asid length to be used in the implementation is also derived by checking legal values of the
    `satp.asid` csr field.
  - The size of the physical address to be implemented is derived from the `physical_addr_sz` node
    of the isa yaml
  - The number of mhpmcounters (and therefore mhpmevents) and their behavior is also captured from
    the csrs defined in the input isa yaml
  - the number of pmp entries and granularity is also captured from the input isa yaml.
  - custom interrupts/exceptions and their cause values are also captured from the input isa yaml.
    The implementation creates an entry in the defines file with for the name and cause value. The
    usage of these custom causes need to be implemented separately in the bsv code.
  - The max size of the cause field in the `mcause` csr is also derived by checking for the max cause
    value being used after accounting for the custom interrupts and exceptions.

Micro-Architectural Configuration hooks
---------------------------------------

The C-Class core has also defined a custom schema to control various micro-architectural features
of the core. A sample configuration file is available in the sample_config directory. 

The following provides a list and description of the configuration hooks available at the
micro-architectural level. Note, there are also hooks in this configuration which control the
bluespec compilation commands and the verilator commands as well.


.. include:: schema_doc.rst

