.. _developers:

##############
For Developers
##############

This section describes the directory structure and other details for folks interested in
hacking/modifying the core/generator scripts.

Directory Structure
-------------------

:: 

 c-class
  ├── bsvpath               # file listing all the directories containing relevant bsv files
  ├── CHANGELOG.rst         # contains the CHANGELOG of versions
  ├── configure             # contains the python configuration scripts
  ├── CONTRIBUTING.md       # guideline for making contributions
  ├── docs                  # all the documentation sources 
  ├── LICENSE.*             # License files
  ├── Makefile              # makefile for compiling bsv files and linking using verilator
  ├── micro-arch-tests      # contains a variety of directed tests 
  ├── README.md             # main doc readme
  ├── rename_translate.sh   # bash script for manipulating verilog files
  ├── requirements.txt      # list of all python packages required for configuring the core
  ├── sample_config         # sample yaml configuration files
  ├── src                   # contains bsv source code of the C-class core
  └── test_soc              # contains a sample test-bench for simulation purposes

Upgrading dependencies
----------------------

The core and test-soc uses modules which are available in different repositories. This list of
repositories is maintained in the configure/constants.py under the variable: ``dependency_yaml``.
The configurator uses the repo-manager package to clone and patch all relevant dependencies.

Changing Compile arguments
--------------------------

The bsc and verilator commands along with their arugments is stored in the configure/constants.py
file under the variables: ``bsc_cmd`` and ``verilator_cmd`` respectively. These are directly used by
the configurator to generate the makefile.inc file.

Adding Checks on YAML
---------------------

The configurator also performs specific checks on the legality of the input yaml. Not all
configurations are legal and this is performed by the function ``specific_checks`` in the
``configure/configure.py`` file. More checks should be added only to this function.
