# Bash completion for Aura

# generates completion line
_aura_compgen() {
    local i r
    COMPREPLY=($( compgen -W "$1" -- "$cur" ))
    for (( i=1; i < ${#COMP_WORDS[@]}-1; i++ )); do
        for r in ${!COMPREPLY[@]}; do
            if [[ ${COMP_WORDS[i]} = ${COMPREPLY[r]} ]]; then
                unset 'COMPREPLY[r]'; break
            fi
        done
    done
    return 0
}

# check if COMP_LINE contains a given string
_aura_incomp() {
    if [[ $COMP_LINE =~ $1 ]]; then
        return 0
    fi
    return 1
}

# deal with options that have an equal sign at the end
# NOTE: regex for username might need tweaking to match
#       Arch Linux username requirements
_aura_has_equal() {
    # to make sure these options haven't been used already
    local b_reg=--build=[^\0]+[[:blank:]]
    local bu_reg=--builduser=[a-z_][-a-z0-9_]*[[:blank:]]
    if [[ $cur == *=* ]]; then
        prev=${cur%%=*}
        cur=${cur#*=}
    fi
    if ! _aura_incomp "$b_reg"; then
        if _aura_incomp --build=; then
            _filedir -d
            return 0
        fi
    fi
    if ! _aura_incomp "$bu_reg"; then
        if _aura_incomp --builduser=; then
            _usergroup
            return 0
        fi
    fi
    return 1
}

# these common pacman options require directory or file completion
_aura_pac_common_opts() {
    local c status regex dir_comm file_comm
    status="1"
    # to make sure these options haven't been used already
    regex=($c[[:blank:]][^\0]+[[:blank:]])
    dir_comm="--cachedir --dbpath --gpgdir --root -b -r"
    file_comm="--config --logfile"
    for c in $dir_comm $file_comm; do
        if [[ $dir_comm == *$c* ]]; then
            if ! _aura_incomp "$regex" && _aura_incomp $c ; then
                _filedir -d
                status="0"
                break
            fi
        elif [[ $file_comm == *$c* ]]; then
            if  ! _aura_incomp "$regex" && _aura_incomp $c ; then
                _filedir
                status="0"
                break
            fi
        fi
    done
    return "$status"
}

# sends list of ABS packages to compgen
_aura_pac_pkg() {
    _aura_compgen "$(
        if [[ $2 ]]; then
            \aura -"$1" 2>/dev/null | \cut -d' ' -f1 | \sort -u
        else
            \aura -"$1" 2>/dev/null
        fi
    )"
    return 0
}

# complete pacman package files
_aura_pac_file() {
    compopt -o filenames
    _filedir 'pkg.tar*'
    return 0
}

_aura()
{
    local cur prev core pcommon o showopts opts
    COMPREPLY=()
    _init_completion || return
    _get_comp_words_by_ref cur prev
    core="-A --aursync -B --save -C --downgrade -L --viewlog -M --abssync \
        -O --orphans -D --database -Q --query -R --remove -S --sync \
        -T --deptest -U --upgrade --auradebug --languages --noconfirm --no-pp \
        -V --version -h --help"
    # options common to all pacman functions
    pcommon="--arch --cachedir --color --config --dbpath --debug --gpgdir \
        --help --logfile --noconfirm --root --verbose -b -h -r -v"

    # check if base option has been selected
    for o in $core; do
        _aura_incomp $o && break
    done

    # if no core option entered
    if [[ $? != 0 ]]; then
        _aura_compgen "$core"
    else
        # show options for core selection when dash typed
        if [[ "$cur" = -* ]]; then
            showopts=true
        else
            showopts=false
        fi
        # main option handling
        case ${o} in
            ##### Aura Options #####
            -A|--aursync)
                if $showopts; then
                    opts="-a --delmakedeps -d --deps -i --info -k --diff -p \
                        --pkgbuild -q --quiet -s --search -u --sysupgrade -w \
                        --downloadonly -x --unsuppress -y --refresh \
                        --aurignore= --build= --builduser= -custom --devel \
                        --hotedit --ignorearch --absdeps --dryrun"
                    # include suboptions when searching
                    local a search_opts="--abc --head --tail"
                    for a in '-As' '-s' '--search'; do
                        if _aura_incomp $a; then
                            _aura_compgen "$opts $search_opts"
                            return 0
                        fi
                    done
                    _aura_compgen "$opts"
                    return 0
                else
                    _aura_has_equal
                    return 0
                fi
                ;;
            -B|--save)
                opts="-c --clean -r --restore"
                _aura_compgen "$opts"
                return 0
                ;;
            -C|--downgrade)
                if $showopts; then
                    opts="-b --backup -c --clean -s --search"
                    _aura_compgen "$opts"
                    return 0
                else
                    local c
                    # complete for dir to backup cache
                    for c in '-Cb' 'b' '--backup'; do
                        if _aura_incomp $c; then
                            _filedir
                            return 0
                        fi
                    done
                fi
                ;;
            -L|--viewlog)
                opts="-i --info -s --search"
                _aura_compgen "$opts"
                return 0
                ;;
            -M|--abssync)
                if $showopts; then
                    opts="-a --delmakedeps -c --clean -d --deps -i --info \
                        -k --diff -p --pkgbuild -s --search -t --treesync \
                        -x --unsuppress -y --refresh --absdeps"
                    _aura_compgen "$opts"
                    return 0
                else
                    # offer ABS pkg selection
                    _aura_pac_pkg Slq
                    return 0
                fi
                ;;
            -O|--orphans)
                if $showopts; then
                    opts="-j --abandon"
                    _aura_compgen "$opts"
                    return 0
                else
                    # use self generated orphan list
                    local o=$( aura -O )
                    if [[ $o != "" ]]; then
                        _aura_compgen "$o"
                        return 0
                    fi
                fi
                ;;
            ##### Pacman Options #####
            -D|--database)
                if $showopts; then
                    opts="--asdeps --asexplicit $pcommon"
                    _aura_compgen "$opts"
                    return 0
                else
                    if _aura_pac_common_opts; then
                        return 0
                    fi
                    _aura_pac_pkg Qq
                    return 0
                fi
                ;;
            -Q|--query)
                if $showopts; then
                    opts="--changelog --check --deps --explicit --file \
                        --foreign --groups --info --list --native --owns \
                        --quiet --search --unrequired --upgrades -c -d -e -g \
                        -i -k -l -m -n -o -p -q -s -t -u $pcommon"
                    _aura_compgen "$opts"
                    return 0
                else
                    local q
                    local ary=('-Qg' '-g' '--groups' '-Qp' '-p' '--file'
                            '-Qo' '-o' '--owns')
                    for q in ${ary[@]}; do
                        if _aura_incomp $q; then
                            case $q in
                                -Qg|-g|--groups)
                                    _aura_pac_pkg Qg sort
                                    ;;
                                -Qp|-p|--file)
                                    _aura_pac_file
                                    ;;
                                -Qo|-o|--owns)
                                    _filedir
                                    ;;
                            esac
                            return 0
                        fi
                    done
                    if _aura_pac_common_opts; then
                        return 0
                    fi
                    _aura_pac_pkg Qq
                    return 0
                fi
                ;;
            -R|--remove)
                if $showopts; then
                    opts="--cascade --dbonly --nodeps --noprogressbar --nosave \
                        --noscriptlet --print --print-format --recursive \
                        --unneeded -c -d -n -p -s -u $pcommon"
                    _aura_compgen "$opts"
                    return 0
                else
                    if _aura_pac_common_opts; then
                        return 0
                    fi
                    _aura_pac_pkg Qq
                    return 0
                fi
                ;;
            -S|--sync)
                if $showopts; then
                    opts="--asdeps --asexplicit --clean --dbonly --downloadonly \
                        --force --groups --ignore --ignoregroup --info --list \
                        --needed --nodeps --noprogressbar --noscriptlet --print \
                        --print-format --quiet --refresh --recursive --search \
                        --sysupgrade -c -d -g -i -l -p -q -s -u -w -y $pcommon"
                    _aura_compgen "$opts"
                    return 0
                else
                    local s ary
                    local ary=('-Sg' '-g' '--groups' '-Sl' '-l' '--list')
                    for s in ${ary[@]}; do
                        if _aura_incomp $s; then
                            case $s in
                                -Sg|-g|--groups)
                                    _aura_pac_pkg Sg sort
                                    ;;
                                -Sl|-l|--list)
                                    _aura_pac_pkg Sl sort
                                    ;;
                            esac
                            return 0
                        fi
                    done
                    if _aura_pac_common_opts; then
                        return 0
                    fi
                    _aura_pac_pkg Slq
                    return 0
                fi
                ;;
            -T|--deptest)
                if $showopts; then
                    _aura_compgen "$pcommon"
                    return 0
                else
                    if _aura_pac_common_opts; then
                        return 0
                    fi
                    _aura_pac_pkg Slq
                    return 0
                fi
                ;;
            -U|--upgrade)
                if $showopts; then
                    opts="--asdeps --asexplicit --dbonly --force --ignore \
                        --ignoregroup --needed --nodeps --noprogressbar \
                        --noscriptlet --print --print-format --recursive \
                        -d -p $pcommon"
                    _aura_compgen "$opts"
                    return 0
                else
                    if _aura_pac_common_opts; then
                        return 0
                    fi
                    _aura_pac_file
                    return 0
                fi
                ;;
            *)
                return 0
                ;;
        esac
    fi
}
complete -F _aura -o nospace aura
