import logging
import os
import shutil
import sys
import subprocess

import configure.configure as configure
import configure.utils as utils


def clone_github_repo(repo_url, clone_dir=None):
    """
    Clone a GitHub repository using subprocess and git.

    Args:
        repo_url (str): GitHub repository URL.
        clone_dir (str): Directory to clone into.

    Returns:
        bool: True if successful, False otherwise.
    """
    try:
        cmd = ["git", "clone", repo_url]
        if clone_dir:
            cmd.append(clone_dir)
        subprocess.run(cmd, check=True)
        return True
    except subprocess.CalledProcessError as e:
        print(f"Error cloning repository: {e}")
        return False


def main():
    '''
        Entry point for riscv_config.
    '''

    # Step 1: Clone GitHub repo
    repo_url = "https://github.com/rishiakrishnan/sample_configure.git"  # TODO: Replace with actual URL
    clone_path = "sample_config"

    if not os.path.exists(clone_path):
        print(f"Cloning GitHub repo from {repo_url} into {clone_path}...")
        if not clone_github_repo(repo_url, clone_path):
            print("Failed to clone GitHub repository.")
            sys.exit(1)
    else:
        print(f"GitHub repo already cloned at {clone_path}")

    # Step 2: Parse CLI arguments and set up logging
    parser = utils.config_cmdline_args()
    args = parser.parse_args()

    utils.setup_logging(args.verbose)
    logger = logging.getLogger()
    logger.handlers = []
    ch = logging.StreamHandler()
    ch.setFormatter(utils.ColoredFormatter())
    logger.addHandler(ch)

    logger.info('************ C-Class Core Generator ************ ')
    logger.info('----------- Copyright (c) IIT Madras ----------- ')
    logger.info('---------- Available under BSD License---------- ')
    logger.info('\n\n')

    if args.clean is None:
        update_dep = True
        patch = True
    else:
        update_dep = False
        patch = False

    if logging:
        logger.info('Checking pre-requisites')
    configure.check_prerequisites()
    configure.handle_dependencies(args.verbose, args.clean, update_dep, patch)
    work_dir = 'build'

    if args.ispec is None:
        logger.error('No ISA YAML provided')
        sys.exit(0)
    elif not args.clean:
        configure.install_riscv_config()
        configure.install_csrbox()
        import csrbox.csr_gen as csr_gen
        from csrbox.errors import ValidationError
        import csrbox
        import riscv_config.checker as checker
        import riscv_config
        logger.info('Using CSRBOX Version: ' + str(csrbox.__version__))
        logger.info('Using RISCV-CONFIG Version: ' + str(riscv_config.__version__))

        logger.info('Validating ISA YAML: ' + str(args.ispec))
        os.makedirs(work_dir, exist_ok=True)

        try:
            isa_file = checker.check_isa_specs(args.ispec, work_dir, True)
        except ValidationError as msg:
            logger.error(msg)
            return 1

        logger.info('Starting CSR generation using CSR-BOX')
        if args.customspec:
            try:
                custom_file = checker.check_custom_specs(args.customspec, work_dir, True)
            except ValidationError as msg:
                logger.error(msg)
                return 1
        else:
            custom_file = None

        if args.dspec:
            try:
                debugfile = checker.check_debug_specs(args.dspec, isa_file, work_dir, True)
            except ValidationError as msg:
                logger.error(msg)
                return 1
        else:
            debugfile = None

        bsv_dir = 'csrbox_bsv/'
        os.makedirs(bsv_dir, exist_ok=True)
        csr_gen.csr_gen(isa_file, args.gspec, custom_file, debugfile, None, bsv_dir, 'soc', False, logging=True)

    if args.cspec is None:
        logger.info('No CORE YAML provided')
        sys.exit(0)
    elif args.clean is None:
        configure.validate_specs(os.path.abspath(args.cspec),
                                 isa_file,
                                 debugfile,
                                 os.path.abspath(args.gspec), True)

    shutil.rmtree("./sample_config")

if __name__ == "__main__":
    exit(main())