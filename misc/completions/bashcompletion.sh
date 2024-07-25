# This file is in the public domain.

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
  local r="[[:space:]]-(-${1#* }[[:space:]]|[[:alnum:]_]*${1% *})"; [[ $COMP_LINE =~ $r ]]
}

_aura_pkg() {
  _arch_compgen "$(
    if [[ $2 ]]; then
      \aura -$1 2>/dev/null | \cut -d' ' -f1 | \sort -u
    else
      \aura -$1 2>/dev/null
    fi
  )"
}

_aura_repo_list() {
  _arch_compgen "$(pacman-conf --repo-list)"
}

_aura() {
  compopt -o default
  local common core aursync cache database files viewlog orphans query remove sync upgrade o
  local cur prev words cword
  _init_completion || return
  aursync=('info open pkgbuild search sysupgrade provides clone' 'i o p s u v w')
  cache=('backup clean info list missing search invalid refresh' 'b c i l m s t y')
  database=('asdeps asexplicit')
  files=('list machinereadable refresh regex' 'l x y')
  viewlog=('info search' 'i s')
  orphans=('adopt abandon elderly' 'a j e')
  query=('changelog check deps explicit file foreign groups info list native owns
          search unrequired upgrades' 'c e g i k l m n o p s t u')
  remove=('cascade dbonly nodeps assume-installed nosave print recursive unneeded' 'c n p s u')
  sync=('asdeps asexplicit clean dbonly downloadonly groups ignore ignoregroup
         info list needed nodeps assume-installed print refresh recursive search sysupgrade'
        'c g i l p s u w y')
  upgrade=('asdeps asexplicit needed nodeps assume-installed print recursive' 'p')
  common=('arch cachedir color config confirm dbpath debug gpgdir help hookdir logfile
           noconfirm noprogressbar noscriptlet quiet root verbose' 'b d h q r v')
  core=('aursync cache database files viewlog orphans help query remove sync upgrade version' 'A C D F L O Q R S U V h')

  for o in 'A aursync' 'C cache' 'D database' 'F files' 'L viewlog' 'O orphans' 'Q query' 'R remove' 'S sync' 'U upgrade'; do
    _arch_incomp "$o" && break
  done

  if [[ $? != 0 ]]; then
    _arch_ptr2comp core
  elif [[ ! $prev =~ ^-[[:alnum:]_]*[Vbhr] &&
    ! $prev = --@(cachedir|color|config|dbpath|help|hookdir|gpgdir|logfile|root|version) ]]
  then
    [[ $cur = -* ]] && _arch_ptr2comp ${o#* } common ||
      case ${o% *} in
      D|R)
          _aura_pkg Qq;;
      F)
        { _arch_incomp 'l list'   && _aura_pkg Slq ; }       ||
          compopt +o default;;
      Q)
        { _arch_incomp 'g groups' && _aura_pkg Qg sort; }    ||
        { _arch_incomp 'p file'   && _aura_file; }           ||
        { _arch_incomp 's search' && compopt +o default; }     ||
        { _arch_incomp 'u upgrades' && compopt +o default; }   ||
          _arch_incomp 'o owns'   ||
          _aura_pkg Qq;;
      S)
        { _arch_incomp 'g groups' && _aura_pkg Sg; }      ||
        { _arch_incomp 'l list'   && _aura_repo_list; } ||
        { _arch_incomp 's search' && compopt +o default; }  ||
          _aura_pkg Slq;;
      U)
          _aura_file;;
      esac
  fi
  true
}

_aura_file() {
  compopt -o filenames; _filedir 'pkg.tar*'
}

complete -F _aura aura

# ex:et ts=2 sw=2 ft=sh
