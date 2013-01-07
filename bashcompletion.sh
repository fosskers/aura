# Bash completion for Aura

_aura() 
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="--languages --noconfirm --conf -A -C -L -O \
          --aursync --downgrade --viewlog --orphans  \
          --save --restore"

    case ${prev} in
	-A|--aursync)
	    opts="-a --delmakedeps -d --deps -k --diff -i --info \
                  -p --pkgbuild -s --search -u --sysupgrade      \
                  -d --downloadonly -x --unsuppress --hotedit"
	    COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
	    return 0
	    ;;
	-C|--downgrade)
	    opts="-b --backup -c --clean -s --search"
	    COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
	    return 0
	    ;;
	-L|--viewlog)
	    opts="-i --info -s --search"
	    COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
	    return 0
	    ;;
	-O|--orphans)
	    opts="-j --abandon"
	    COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
	    return 0
	    ;;
	*)
	;;
    esac

    if [[ ${cur} == -* ]] ; then
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
    fi
}

complete -F _aura aura
