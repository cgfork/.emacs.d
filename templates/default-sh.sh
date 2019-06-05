#! /bin/bash
#================================================================
# HEADER
#================================================================
#% USAGE
#+    ${SCRIPT_NAME} [OPTIONS]
#%
#% DESCRIPTION
#%    This is a script template
#%
#% OPTIONS
#%    -h, --help                    Print this help
#%    -v, --version                 Print script information
#%
#% EXAMPLES
#%    ${SCRIPT_NAME} -h
#%
#================================================================
#- IMPLEMENTATION
#-    version         ${SCRIPT_NAME} 0.0.1
#-    author          cgfork
#-
#================================================================
#  HISTORY
#     2019/06/03 : cgfork : Script creation
# 
#================================================================
#  DEBUG OPTION
#    set -n  # Uncomment to check your syntax, without execution.
#    set -x  # Uncomment to debug this shell script
#
#================================================================
# END_OF_HEADER
#================================================================

SCRIPT_HEADSIZE=$(head -200 ${0} |grep -n "^# END_OF_HEADER" | cut -f1 -d:)
SCRIPT_NAME="$(basename ${0})"

function usage() {
    head -${SCRIPT_HEADSIZE:-99} ${0} | grep -e "^#[%+-]" | sed -e "s/^#[%+-]//g" -e "s/\${SCRIPT_NAME}/${SCRIPT_NAME}/g" ;
}

function info() {
    head -${SCRIPT_HEADSIZE:-99} ${0} | grep -e "^#-" | sed -e "s/^#-//g" -e "s/\${SCRIPT_NAME}/${SCRIPT_NAME}/g";
}

GETOPT="/usr/local/opt/gnu-getopt/bin/getopt"

ARGS=`${GETOPT} -o hv -l help,version -n 'getopt_error.sh' -- "${@}"`

if [ ${?} != 0 ]; then
    usage
    exit 1
fi

eval set -- "${ARGS}"

while true; do
    case ${1} in
	-h|--help)
	    usage;
	    exit 1
	    ;;
	-v|--version)
	    info;
	    exit 1
	    ;;
	--)
	    shift;
	    break
	    ;;
	*)
	    usage;
	    exit 1
	    ;;
    esac
done

for arg in $@
do
    echo "processing $arg"
done
