# set fish_plugins emacs

function fish_prompt
  # virtualfish
  if set -q VIRTUAL_ENV
    echo -n -s (set_color -o blue) "(" (basename "$VIRTUAL_ENV") ")" (set_color normal) " "
  end

  # make a neat gradient for the directories on our working directory
  set colors 444 666 888 aaa
  set dirs (prompt_pwd | string split "/")
  set num_dirs (count $dirs)

  if set -q SSH_CONNECTION
     echo -n -s (set_color -o green) "[" (hostname) "] "
  end

  for x in (seq $num_dirs)
    set col_idx (math (count $colors) - (math $num_dirs - $x))
    if test $col_idx -lt 1
      set col_idx 1
    end

    set_color -o $colors[$col_idx]
    printf '%s' $dirs[$x]

    if test $x -lt $num_dirs
      printf '/'
    end

    set_color normal
  end

  # display git branch name if we are in a git work tree
  begin
    set temp (mktemp)
    set is_git (git rev-parse --is-inside-work-tree 2>&1 >$temp)
    if test $status -eq 0
      set_color $colors[(math (count $colors) - 2)]
      printf ' %s' (git rev-parse --abbrev-ref HEAD)
    end
  end

  # visual separation between the prompt and user input
  set_color -o $colors[(count $colors)]
  printf ' · '

  set_color normal
end

# TRAMP workaround
if test "$TERM" = "dumb"
  function fish_prompt
    echo "\$ "
  end

  function fish_right_prompt; end
  function fish_greeting; end
  function fish_title; end
end

alias clear-pycache="find . | grep -E \"(__pycache__|.pyc|.pyo)\" | xargs rm -rf"

alias ls="exa -h --git --group-directories-first"
alias lm="exa -h --git --group-directories-first -lmr --sort modified"
alias lua="lua5.3"

alias rg="rg -S"

# verilator
alias verilator="perl -wS verilator"# pyenv
set -Ux PYENV_ROOT $HOME/.pyenv
set -Ux fish_user_paths $PYENV_ROOT/bin $fish_user_paths
status --is-interactive; and pyenv init - | source
status --is-interactive; and pyenv virtualenv-init - | source

