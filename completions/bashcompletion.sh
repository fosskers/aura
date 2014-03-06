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
 
_arch_compgen() {
    local i r
    COMPREPLY=($(compgen -W '$*' -- "$cur"))
    for ((i=1; i < ${#COMP_WORDS[@]}-1; i++)); do
        for r in ${!COMPREPLY[@]}; do
            if [[ ${COMP_WORDS[i]} = ${COMPREPLY[r]} ]]; then
                unset 'COMPREPLY[r]'; break
            fi
        done
    done
}

_arch_ptr2comp() {
    local list= x y
        for x; do
            for y in '0 --' '1 -'; do
                eval 'set -- ${'$x'[${y% *}]}'
                list+=\ ${@/#/${y#* }}
            done
        done
    _arch_compgen $list
}

_arch_incomp() {
    local r="\s-(-${1#* }\s|\w*${1% *})"; [[ $COMP_LINE =~ $r ]]
}

_aura_pacman_pkg() {
    _arch_compgen "$(
        if [[ $2 ]]; then
            \aura -$1 2>/dev/null | \cut -d' ' -f1 | \sort -u
        else
            \aura -$1 2>/dev/null
        fi
    )"
}

_aura_pacman() {
    local common core cur database prev query remove sync upgrade o
    COMPREPLY=()
    _get_comp_words_by_ref cur prev
    database=('asdeps asexplicit')
    query=('changelog check deps explicit file foreign groups info list owns
            search unrequired upgrades' 'c e g i k l m o p s t u')
    remove=('cascade dbonly nodeps nosave print recursive unneeded' 'c n p s u')
    sync=('asdeps asexplicit clean dbonly downloadonly force groups ignore ignoregroup
            info list needed nodeps print refresh recursive search sysupgrade'
            'c g i l p s u w y')
    upgrade=('asdeps asexplicit force needed nodeps print recursive' 'p')
    common=('arch cachedir color config dbpath debug help logfile noconfirm
            noprogressbar noscriptlet quiet root verbose' 'b d h q r v')
    core=('database help query remove sync upgrade version' 'D Q R S U V h')

    for o in 'D database' 'Q query' 'R remove' 'S sync' 'U upgrade'; do
        _arch_incomp "$o" && break
    done

    if [[ $? != 0 ]]; then
        _arch_ptr2comp core
    elif [[ ! $prev =~ ^-\w*[Vbhr] &&
        ! $prev = --@(cachedir|color|config|dbpath|help|logfile|root|version) ]]
    then
        [[ $cur = -* ]] && _arch_ptr2comp ${o#* } common ||
        case ${o% *} in
            D|R)
                _aura-pacman_pkg Qq;;
            Q)
                { _arch_incomp 'g groups' && _aura-pacman_pkg Qg sort; }    ||
                { _arch_incomp 'p file'   && _aura-pacman_file; }           ||
                _arch_incomp 'o owns'   || _arch_incomp 'u upgrades' ||
                _aura-pacman_pkg Qq;;
            S)
                { _arch_incomp 'g groups' && _aura-pacman_pkg Sg; }      ||
                { _arch_incomp 'l list'   && _aura-pacman_pkg Sl sort; } ||
                _aura-pacman_pkg Slq;;
            U)
                _aura-pacman_file;;
        esac
    fi
    true
}

_aura_pacman_file() {
    compopt -o filenames; _filedir 'pkg.tar*'
}
 
complete -F _aura aura
complete -F _aura-pacman -o default aura