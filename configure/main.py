import logging
import os
import shutil
import sys

import configure.configure as configure
import configure.utils as utils



def main():
    '''
        Entry point for riscv_config.
    '''

    # Set up the parser
    parser = utils.config_cmdline_args()
    args = parser.parse_args()

    # Set up the logger
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
    configure.handle_dependencies(args.verbose, args.clean,update_dep,patch)
    work_dir= 'build'
    if args.ispec is None:
        logger.error('No ISA YAML provided')
        sys.exit(0)
    elif not args.clean:
        configure.install_riscv_config()
        configure.install_csrbox()
        import csrbox.csr_gen as csr_gen
        from   csrbox.errors import ValidationError
        import csrbox
        import riscv_config.checker as checker
        import riscv_config
        logger.info('Using CSRBOX Version: '+ str(csrbox.__version__))
        logger.info('Using RISCV-CONFIG Version: '+ str(riscv_config.__version__))

        logger.info('Validating ISA YAML: ' + str(args.ispec))               
        os.makedirs(work_dir, exist_ok= True)
        #isa_file = riscv_config.check_isa_specs(os.path.abspath(args.ispec), work_dir, False)
        try:
            isa_file = checker.check_isa_specs(args.ispec, work_dir, True)
        except ValidationError as msg:
            logger.error(msg)
            return 1
            
        logger.info('Starting CSR generation using CSR-BOX')
        if args.customspec:
            try:
                custom_file = checker.check_custom_specs(args.customspec,
                        work_dir, True)
            except ValidationError as msg:
                logger.error(msg)
                return 1
        else:
            custom_file = None

        if args.dspec:
            try:
                debugfile = checker.check_debug_specs(args.dspec, isa_file, work_dir,
                        True)
            except ValidationError as msg:
                logger.error(msg)
                return 1
        else:
            debugfile = None

        
        bsv_dir = 'csrbox_bsv/'
                      
        os.makedirs(bsv_dir, exist_ok= True)
        csr_gen.csr_gen(isa_file, args.gspec, custom_file, debugfile, None, bsv_dir, 'soc',False,logging=True)

    if args.cspec is None:
        logger.info('No CORE YAML provided')
        sys.exit(0)
    elif args.clean is None:
        configure.validate_specs(os.path.abspath(args.cspec),
                                 isa_file, 
                                 debugfile, 
                                 os.path.abspath(args.gspec), True)

if __name__ == "__main__":
    exit(main())
