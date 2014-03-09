# Bash completion for Aura

_aura() 
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="--languages --noconfirm --conf -A -B -C -L -O \
          --aursync --save --downgrade --viewlog --orphans \
          --no-pp"

    case ${prev} in
        -A|--aursync)
            opts="-a --delmakedeps -d --deps -k --diff -i --info       \
                  -p --pkgbuild -q --quiet -s --search -u --sysupgrade \
                  -d --downloadonly -x --unsuppress --hotedit          \
                  --aurignore="
            COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
            return 0
            ;;
        -B|--save)
            opts="-c --clean -r --restore"
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
