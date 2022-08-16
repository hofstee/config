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
      printf ' %s' (git rev-parse --abbrev-ref HEAD 2>/dev/null)
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

# # pyenv
# set -x PATH "/home/teguhhofstee/.pyenv/bin" $PATH
# status --is-interactive; and . (pyenv init -|psub)
# status --is-interactive; and . (pyenv virtualenv-init -|psub)

# verilator
alias verilator="perl -wS verilator"

# wsl2 x11 forwarding
if set -q WSL_INTEROP
   export DISPLAY=(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0.0
end

# nix
bass source ~/.nix-profile/etc/profile.d/nix{,-daemon}.sh

set -x PATH "$HOME/.local/bin" $PATH

# vtune
bass source /opt/intel/vtune_profiler/env/vars.sh

# rust
source "$HOME/.cargo/env"
