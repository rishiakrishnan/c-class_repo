###########
Quick Start
###########


For this quick-start you will need the following tools. The user is requested to install
these from the respective repositories/sources:

* `Bluespec Compiler <https://github.com/B-Lang-org/bsc>`__ : Make sure you are using the version
  post **April 26 2020**
* `Verilator <https://www.veripool.org/projects/verilator/wiki/Installing>`__
* `RISC-V GNU ToolChain <https://github.com/riscv/riscv-gnu-toolchain>`__
* `Modified RISC-V ISA Sim <https://gitlab.com/shaktiproject/tools/mod-spike/-/tree/bump-to-latest>`__
* `RISC-V OpenOCD <https://github.com/riscv/riscv-openocd>`__
* DTC 1.4.7: see dtc_
* Python 3.7.0: see python_

.. warning:: The following few sections are a quick copy-paste of the steps to install the above tools. 
   However,it is possible that these steps are outdated as either the repository has shifted or the master of
   the respective repositories now have moved forward with new dependencies or installation procedures.
   We thereby suggest refering to the original repositories of the above tools to install them.

If you already have the above tools installed you can directly jump to building your core: build_

.. _python:

Install Python Dependencies
---------------------------

The core generator requires ``pip`` and ``python`` (>=3.7) to be available on
your system. If you have issues installing, either of these, directly on your system we
suggest using a virtual environment like `pyenv` to make things easy.


First Install the required libraries/dependencies:

.. code-block:: bash

    $ sudo apt-get install -y make build-essential libssl-dev zlib1g-dev libbz2-dev \
        libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev \
        xz-utils tk-dev libffi-dev liblzma-dev python-openssl git

Next, install `pyenv`

.. code-block:: bash

  $ curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash

Add the following to your `.bashrc` with appropriate changes to username:

.. code-block:: bash

  export PATH="/home/<username>/.pyenv/bin:$PATH"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"

Open a new terminal and create a new python virtual environment:

.. code-block:: bash

  $ pyenv install 3.7.0
  $ pyenv virtualenv 3.7.0 myenv

Now you can activate this environment in any other terminal :

.. code-block:: bash

  pyenv activate myenv
  python --version

.. _dtc:

Install DTC (device tree compiler)
----------------------------------

We use the DTC 1.4.7 to generate the device tree string in the boot-files. 
To install DTC follow the below commands:

.. code-block:: bash

  sudo wget https://git.kernel.org/pub/scm/utils/dtc/dtc.git/snapshot/dtc-1.4.7.tar.gz                
  sudo tar -xvzf dtc-1.4.7.tar.gz                                                                     
  cd dtc-1.4.7/                                                                                       
  sudo make NO_PYTHON=1 PREFIX=/usr/                                                                  
  sudo make install NO_PYTHON=1 PREFIX=/usr/                                                          

.. _build:

Building the Core
-----------------

The code is hosted on Gitlab and can be checked out using the following
command:

.. code-block:: bash

  $ git clone https://gitlab.com/shaktiproject/cores/c-class.git

If you are cloning the c-class repo for the first time it would be best to install the dependencies
first:

.. code-block:: bash

  $ cd c-class/
  $ pyenv activate venv # ignore this is you are not using pyenv
  $ pip install -U -r c-class/requirements.txt

The C-class core generator takes a specific :ref:`YAML<configure-core-label>` format as input. It makes specific checks to
validate if the user has entered valid data and none of the parameters conflict with each other.
For e.g., mentioning the 'D' extension without the 'F' will get captured by the generator as an
invalid spec. More information on the exact parameters and constraints on each field are discussed
here.

Once the input YAML has been validated, the generator then clones all the dependent repositories
which enable building a test-soc, simulating it and performing verification of the core. 
This is an alternative to maintaining the repositories as submodules, which
typically pollutes the commit history with bump commits.

At the end, the generator outputs a single ``makefile.inc`` in the same folder that it was run,
which contains definitions of paths where relevant bluespec files are present, bsc command with
macro definitions, verilator simulation commands, etc.

A sample yaml input YAML (`default.yaml`) is available in the ``sample_config`` directory of the
repository. 

To build the core with a sample test-soc using the default config do the following:

.. code-block:: bash

  $ python -m configure.main -ispec sample_config/default.yaml

The above step generates a ``makefile.inc`` file in the same folder and also
clones other dependent repositories to build a test-soc and carry out
verification. This should generate a log something similar to:

.. code-block:: text

  [INFO]    : ************ C-Class Core Generator ************ 
  [INFO]    :            Available under BSD License
  
  
  [INFO]    : [update] Cloning caches_mmu ...
                ...
                ...
                ...
  [INFO]    : Loading input file: ..../sample_config/default.yaml
  [INFO]    : Load Schema configure/schema.yaml
  [INFO]    : Initiating Validation
  [INFO]    : No Syntax errors in Input Yaml.
  [INFO]    : Performing Specific Checks
  [INFO]    : Generating BSC compile options
  [INFO]    : makefile.inc generated

To compile the bluespec source and generate verilog:

.. code-block:: bash

  $ make

This should generate the following folders:

1. verilog: contains the verilofg files generated by bsc
2. bsv_build: contains all the intermediate and information files generated by bsc
3. bin: contains final verilated executable :``out`` which is used for simulation along with some 
   boot and application hex files.

.. note:: To leverage parallel builds you can do the following:

   .. code-block:: bash

      make -j<jobs> generate_verilog; make generate_boot_files link_verilator

Run Smoke Tests
---------------

You can run the individual riscv-tests on the generated verilog of the test-soc using the following:

.. code-block:: bash

  $ make test opts='--test=add --suite=rv64ui ' CONFIG_ISA=RV64IMAFDC

You can run the entire riscv-tests suite in a regression using the following: :

.. code-block:: bash

  $ make regress opts='--filter=rv64 --parallel=20 --sub' CONFIG_ISA=RV64IMAFDC
  $ make regress opts='--filter=rv64 --final'

The last command, after some delay, should present the following output:

.. code-block:: bash

     recoding                                   rv64uf     v    PASSED
          slt                                   rv64ui     p    PASSED
         fadd                                   rv64uf     v    PASSED
          and                                   rv64ui     p    PASSED
       fcvt_w                                   rv64uf     v    PASSED
     amoadd_d                                   rv64ua     p    PASSED
        fmadd                                   rv64ud     p    PASSED
         ldst                                   rv64uf     v    PASSED
     amoand_d                                   rv64ua     p    PASSED
         fmin                                   rv64ud     p    PASSED
           lh                                   rv64ui     v    PASSED
    amomaxu_w                                   rv64ua     v    PASSED
     amoand_w                                   rv64ua     p    PASSED
     amoxor_d                                   rv64ua     v    PASSED
      fence_i                                   rv64ui     v    PASSED
          bne                                   rv64ui     p    PASSED
     amomin_d                                   rv64ua     v    PASSED
       fcvt_w                                   rv64uf     p    PASSED
         srli                                   rv64ui     p    PASSED
           sw                                   rv64ui     v    PASSED
    amomaxu_d                                   rv64ua     v    PASSED
         lrsc                                   rv64ua     v    PASSED
        fmadd                                   rv64ud     v    PASSED
          blt                                   rv64ui     v    PASSED
         fadd                                   rv64ud     p    PASSED
     recoding                                   rv64uf     p    PASSED
           sh                                   rv64ui     v    PASSED
          ori                                   rv64ui     p    PASSED
         fdiv                                   rv64uf     v    PASSED
      ma_addr                                   rv64mi     p    PASSED
     recoding                                   rv64ud     p    PASSED
          add                                   rv64ui     p    PASSED
          blt                                   rv64ui     p    PASSED
       fcvt_w                                   rv64ud     p    PASSED
         bltu                                   rv64ui     v    PASSED
          sll                                   rv64ui     v    PASSED
     ma_fetch                                   rv64mi     p    PASSED
          jal                                   rv64ui     p    PASSED
          lwu                                   rv64ui     p    PASSED
           sd                                   rv64ui     v    PASSED
          ori                                   rv64ui     v    PASSED
       access                                   rv64mi     p    PASSED
           sw                                   rv64ui     p    PASSED
          srl                                   rv64ui     p    PASSED
         fcvt                                   rv64ud     v    PASSED
        fmadd                                   rv64uf     v    PASSED
     amoxor_w                                   rv64ua     v    PASSED
           sb                                   rv64ui     v    PASSED
        slliw                                   rv64ui     p    PASSED
     amoadd_d                                   rv64ua     v    PASSED
         fdiv                                   rv64ud     p    PASSED
           lw                                   rv64ui     v    PASSED
         slti                                   rv64ui     p    PASSED
          add                                   rv64ui     v    PASSED
     amomax_d                                   rv64ua     v    PASSED
         move                                   rv64ud     v    PASSED
          lhu                                   rv64ui     v    PASSED
         andi                                   rv64ui     p    PASSED
        addiw                                   rv64ui     v    PASSED
    amoswap_d                                   rv64ua     v    PASSED
         fdiv                                   rv64ud     v    PASSED
          lui                                   rv64ui     p    PASSED
         ldst                                   rv64uf     p    PASSED
         fmin                                   rv64uf     v    PASSED
     amoxor_w                                   rv64ua     p    PASSED
         srai                                   rv64ui     p    PASSED
         addi                                   rv64ui     p    PASSED
         subw                                   rv64ui     p    PASSED
           sd                                   rv64ui     p    PASSED
     amoand_d                                   rv64ua     v    PASSED
          sra                                   rv64ui     p    PASSED
          rvc                                   rv64uc     v    PASSED
        scall                                   rv64mi     p    PASSED
          beq                                   rv64ui     p    PASSED
          rvc                                   rv64uc     p    PASSED
         fmin                                   rv64ud     v    PASSED
     amoadd_w                                   rv64ua     p    PASSED
        scall                                   rv64si     p    PASSED
         fcmp                                   rv64uf     p    PASSED
        srliw                                   rv64ui     p    PASSED
        addiw                                   rv64ui     p    PASSED
     amomax_w                                   rv64ua     p    PASSED
         andi                                   rv64ui     v    PASSED
         addi                                   rv64ui     v    PASSED
          lhu                                   rv64ui     p    PASSED
          xor                                   rv64ui     p    PASSED
      amoor_w                                   rv64ua     p    PASSED
          and                                   rv64ui     v    PASSED
          lbu                                   rv64ui     v    PASSED
        dirty                                   rv64si     p    PASSED
         ldst                                   rv64ud     v    PASSED
          bge                                   rv64ui     p    PASSED
      amoor_w                                   rv64ua     v    PASSED
           sh                                   rv64ui     p    PASSED
    amoswap_w                                   rv64ua     p    PASSED
     amoxor_d                                   rv64ua     p    PASSED
         fadd                                   rv64uf     p    PASSED
          sll                                   rv64ui     p    PASSED
     amoand_w                                   rv64ua     v    PASSED
     ma_fetch                                   rv64si     p    PASSED
        sraiw                                   rv64ui     p    PASSED
          csr                                   rv64si     p    PASSED
         ldst                                   rv64ud     p    PASSED
    amoswap_w                                   rv64ua     v    PASSED
         bltu                                   rv64ui     p    PASSED
           ld                                   rv64ui     v    PASSED
         fmin                                   rv64uf     p    PASSED
         slli                                   rv64ui     v    PASSED
         fadd                                   rv64ud     v    PASSED
         addw                                   rv64ui     v    PASSED
           lb                                   rv64ui     p    PASSED
    amominu_d                                   rv64ua     p    PASSED
       fcvt_w                                   rv64ud     v    PASSED
         move                                   rv64uf     p    PASSED
          bge                                   rv64ui     v    PASSED
           or                                   rv64ui     p    PASSED
         srlw                                   rv64ui     p    PASSED
         xori                                   rv64ui     p    PASSED
   structural                                   rv64ud     v    PASSED
         sllw                                   rv64ui     p    PASSED
     amomax_d                                   rv64ua     p    PASSED
         fcvt                                   rv64uf     p    PASSED
      amoor_d                                   rv64ua     p    PASSED
    amomaxu_d                                   rv64ua     p    PASSED
         fdiv                                   rv64uf     p    PASSED
           sb                                   rv64ui     p    PASSED
          jal                                   rv64ui     v    PASSED
         addw                                   rv64ui     p    PASSED
    amomaxu_w                                   rv64ua     p    PASSED
        auipc                                   rv64ui     p    PASSED
          bne                                   rv64ui     v    PASSED
    amoswap_d                                   rv64ua     p    PASSED
           lw                                   rv64ui     p    PASSED
         bgeu                                   rv64ui     v    PASSED
     recoding                                   rv64ud     v    PASSED
       simple                                   rv64ui     p    PASSED
           or                                   rv64ui     v    PASSED
          lbu                                   rv64ui     p    PASSED
     amomax_w                                   rv64ua     v    PASSED
         move                                   rv64ud     p    PASSED
       fclass                                   rv64uf     p    PASSED
         jalr                                   rv64ui     p    PASSED
       fclass                                   rv64ud     v    PASSED
        sltiu                                   rv64ui     p    PASSED
         fcmp                                   rv64ud     p    PASSED
         sltu                                   rv64ui     p    PASSED
   structural                                   rv64ud     p    PASSED
           lb                                   rv64ui     v    PASSED
         fcvt                                   rv64uf     v    PASSED
     amomin_d                                   rv64ua     p    PASSED
          sub                                   rv64ui     p    PASSED
          wfi                                   rv64si     p    PASSED
           ld                                   rv64ui     p    PASSED
      amoor_d                                   rv64ua     v    PASSED
         fcvt                                   rv64ud     p    PASSED
         lrsc                                   rv64ua     p    PASSED
       fclass                                   rv64uf     v    PASSED
       fclass                                   rv64ud     p    PASSED
         sraw                                   rv64ui     p    PASSED
     amomin_w                                   rv64ua     v    PASSED
         bgeu                                   rv64ui     p    PASSED
         move                                   rv64uf     v    PASSED
     amoadd_w                                   rv64ua     v    PASSED
      fence_i                                   rv64ui     p    PASSED
           lh                                   rv64ui     p    PASSED
          csr                                   rv64mi     p    PASSED
       simple                                   rv64ui     v    PASSED
          lui                                   rv64ui     v    PASSED
          lwu                                   rv64ui     v    PASSED
         fcmp                                   rv64ud     v    PASSED
          beq                                   rv64ui     v    PASSED
        auipc                                   rv64ui     v    PASSED
    amominu_w                                   rv64ua     p    PASSED
        fmadd                                   rv64uf     p    PASSED
    amominu_w                                   rv64ua     v    PASSED
     amomin_w                                   rv64ua     p    PASSED
         fcmp                                   rv64uf     v    PASSED
         jalr                                   rv64ui     v    PASSED
         slli                                   rv64ui     p    PASSED
    amominu_d                                   rv64ua     v    PASSED
          div                                   rv64um     p    PASSED
          mul                                   rv64um     p    PASSED
        remuw                                   rv64um     p    PASSED
         divw                                   rv64um     p    PASSED
         remw                                   rv64um     p    PASSED
        mulhu                                   rv64um     p    PASSED
         mulw                                   rv64um     p    PASSED
          rem                                   rv64um     p    PASSED
         remu                                   rv64um     p    PASSED
         mulh                                   rv64um     p    PASSED
        divuw                                   rv64um     p    PASSED
       mulhsu                                   rv64um     p    PASSED
         divu                                   rv64um     p    PASSED
         divu                                   rv64um     v    PASSED
        sltiu                                   rv64ui     v    PASSED
          xor                                   rv64ui     v    PASSED
         subw                                   rv64ui     v    PASSED
         mulw                                   rv64um     v    PASSED
         srli                                   rv64ui     v    PASSED
        slliw                                   rv64ui     v    PASSED
          div                                   rv64um     v    PASSED
          sub                                   rv64ui     v    PASSED
         srlw                                   rv64ui     v    PASSED
         sltu                                   rv64ui     v    PASSED
         xori                                   rv64ui     v    PASSED
         remw                                   rv64um     v    PASSED
          mul                                   rv64um     v    PASSED
          slt                                   rv64ui     v    PASSED
          sra                                   rv64ui     v    PASSED
         divw                                   rv64um     v    PASSED
         srai                                   rv64ui     v    PASSED
        mulhu                                   rv64um     v    PASSED
        remuw                                   rv64um     v    PASSED
          srl                                   rv64ui     v    PASSED
          rem                                   rv64um     v    PASSED
       mulhsu                                   rv64um     v    PASSED
         slti                                   rv64ui     v    PASSED
        srliw                                   rv64ui     v    PASSED
         remu                                   rv64um     v    PASSED
        divuw                                   rv64um     v    PASSED
         sllw                                   rv64ui     v    PASSED
         sraw                                   rv64ui     v    PASSED
         mulh                                   rv64um     v    PASSED
        sraiw                                   rv64ui     v    PASSED

Congratulations - You have built your very first C-Class core !! :)
